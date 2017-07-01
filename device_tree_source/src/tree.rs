//! Contains the structures that represent the device tree.

use std::fmt;
use std::collections::HashMap;
use std::collections::hash_map::Entry;

/// Trait applied to all data structures in a device tree that can have a
/// label/alias.
pub trait Labeled {
    /// Add a label to an object.
    ///
    /// #Errors
    /// May fail if a variation of an object does not allow labels.
    // TODO: code examples
    fn add_label(&mut self, label: &str) -> Result<(), ()>;

    /// Gets a slice containing all the labels that point to the object. May
    /// return an empty slice if there are no labels or the object does not
    /// allow labels in its current form, such as will a deleted Node.
    fn get_labels(&self) -> &[String];
}

/// Trait for objects that track their starting offset from within the global
/// buffer.
pub trait Offset {
    /// Get the starting offset of the object.
    fn get_offset(&self) -> usize;
}

/// The device tree info as specified by the Device Tree Specification. Includes
/// the reserved memory info, the boot CPU ID, and the root node of the tree.
/// The root node may not include any changes made by node specifications listed
/// after the root node. These amendments are returned separately. See
/// `parser::parse_dt`.
#[derive(Debug, Clone)]
pub struct DTInfo {
    /// The reserved memory information.
    pub reserve_info: Vec<ReserveInfo>,
    /// The boot CPU ID as specified by the device tree. Defaults to the first
    /// CPU ID if not specified.
    pub boot_cpuid: u32,
    /// The root node of the device tree. Will always be named '/'.
    pub root: Node,
}

impl DTInfo {
    /// Create a new `DTInfo` where the tree is a merging of the original
    /// `DTInfo`'s tree and the list of `Node`s. The original tree is left
    /// unmodified.
    ///
    /// The 'Nodes' will be applied to the tree by fist finding the 'Node' they
    /// reference, either by path or label, and then merging the existing 'Node'
    /// and the 'Node' from the list. This merge is done via 'Node::merge`.
    pub fn apply_amends(&self, amends: &[Node]) -> Self {
        let mut new_tree: DTInfo = self.clone();
        new_tree.merge_amends(amends);
        new_tree
    }

    /// Merge a list of 'Nodes' into the `DTInfo`'s tree, modifying it.
    ///
    /// The 'Nodes' will be applied to the tree by fist finding the 'Node' they
    /// reference, either by path or label, and then merging the existing 'Node'
    /// and the 'Node' from the list. This merge is done via 'Node::merge`.
    pub fn merge_amends(&mut self, amends: &[Node]) {
        for a in amends {
            match a.name() {
                &NodeName::Ref(ref refr) => {
                    // could be either label ref or path ref
                    if refr.starts_with('/') {
                        // path ref
                        self.get_node_by_path_mut(refr).unwrap().merge(a)
                    } else {
                        // label ref, time to get searching
                        self.find_node_by_label_mut(refr).unwrap().merge(a)
                    };
                }
                &NodeName::Full(_) => {
                    // has to be modifying from root node
                    // TODO: double check
                    self.root.merge(a)
                }
            }
        }

        // TODO: recalculate boot CPU ID
    }

    /// Get a reference to a `Node` in the tree by it's path.
    ///
    /// # Errors
    /// Returns an error if no node exists at the specified path exists or if
    /// string passed is not a valid path.
    pub fn get_node_by_path<'a>(&'a self, path: &str) -> Result<&'a Node, ()> {
        fn internal<'a>(node: &'a Node, path: &str) -> Result<&'a Node, ()> {
            if path.is_empty() {
                return Err(())
            }

            let (name, rem) = path.find('/')
                                  .and_then(|pos| Some(path.split_at(pos)))
                                  .and_then(|(a,b)| Some((a, Some(b))))
                                  .unwrap_or((path, None));

            let subnode = match *node {
                Node::Deleted{..} => return Err(()),
                Node::Existing{ref children, ..} => children.get(name)
            };

            match subnode {
                None => Err(()),
                Some(subnode) =>
                    match rem {
                        None => Ok(subnode),
                        Some(path) => internal(subnode, path),
                    },
            }
        }

        if !path.starts_with('/') {
            Err(())
        } else if path == "/" {
            Ok(&self.root)
        } else {
            internal(&self.root, &path[1..])
        }
    }

    /// Get a mutable reference to a `Node` in the tree by it's path.
    ///
    /// # Errors
    /// Returns an error if no node exists at the specified path exists or if
    /// string passed is not a valid path.
    pub fn get_node_by_path_mut<'a>(&'a mut self, path: &str) -> Result<&'a mut Node, ()> {
        fn internal<'a>(node: &'a mut Node, path: &str) -> Result<&'a mut Node, ()> {
            if path.is_empty() {
                return Err(())
            }

            let (name, rem) = path.find('/')
                                  .and_then(|pos| Some(path.split_at(pos)))
                                  .and_then(|(a,b)| Some((a, Some(b))))
                                  .unwrap_or((path, None));

            let subnode = match *node {
                Node::Deleted{..} => return Err(()),
                Node::Existing{ref mut children, ..} => children.get_mut(name)
            };

            match subnode {
                None => Err(()),
                Some(subnode) =>
                    match rem {
                        None => Ok(subnode),
                        Some(path) => internal(subnode, path),
                    },
            }
        }

        if !path.starts_with('/') {
            Err(())
        } else if path == "/" {
            Ok(&mut self.root)
        } else {
            internal(&mut self.root, &path[1..])
        }
    }

    /// Get a reference to a Node pointed to by a label.
    ///
    /// This does a naive depth first search at the moment and has no cache
    /// between runs. This should be fine for a few random accesses, but doing
    /// some sort of manual caching is suggested if searching my labels is
    /// needed often.
    ///
    /// # Errors
    /// Returns an error if the label is empty or if no 'Node' with the label is
    /// found.
    pub fn find_node_by_label<'a>(&'a self, label: &str) -> Result<&'a Node, ()> {
        fn internal<'a>(node: &'a Node, label: &str) -> Result<&'a Node, ()> {
            match *node {
                Node::Deleted { .. } => Err(()),
                Node::Existing { ref labels, ref children, .. } => {
                    if labels.iter().map(|s| s.as_str()).any(|l| l == label) {
                        Ok(node)
                    } else {
                        for child in children.values() {
                            if let Ok(node) = internal(child, label) {
                                return Ok(node)
                            }
                        }

                        Err(())
                    }
                }
            }
        }

        if label.is_empty() {
            Err(())
        } else {
            internal(&self.root, label)
        }
    }

    /// Get a mutable reference to a Node pointed to by a label.
    ///
    /// This does a naive depth first search at the moment and has no cache
    /// between runs. This should be fine for a few random accesses, but doing
    /// some sort of manual caching is suggested if searching my labels is
    /// needed often.
    ///
    /// # Errors
    /// Returns an error if the label is empty or if no 'Node' with the label is
    /// found.
    pub fn find_node_by_label_mut<'a>(&'a mut self, label: &str) -> Result<&'a mut Node, ()> {
        fn internal<'a>(node: &'a mut Node, label: &str) -> Result<&'a mut Node, ()> {
            if node.get_labels().iter().map(|s| s.as_str()).any(|l| l == label) {
                return Ok(node)
            }

            match *node {
                Node::Deleted { .. } => Err(()),
                Node::Existing { ref mut children, .. } => {
                    for child in children.values_mut() {
                        if let Ok(node) = internal(child, label) {
                            return Ok(node)
                        }
                    }

                    Err(())
                }
            }
        }

        if label.is_empty() {
            Err(())
        } else {
            internal(&mut self.root, label)
        }
    }
}

/// Stores the information from a `/memreserve/` statement.
#[derive(Debug, Clone)]
pub struct ReserveInfo {
    /// The starting address of the reserved memory section.
    pub address: u64,
    /// The length in bytes of the reserved memory section.
    pub size: u64,
    /// All labels to the `/memreserve/` statement.
    pub labels: Vec<String>,
}

impl Labeled for ReserveInfo {
    fn add_label(&mut self, label: &str) -> Result<(), ()> {
        let label = label.to_owned();
        if !self.labels.contains(&label) {
            self.labels.push(label);
        }

        Ok(())
    }

    fn get_labels(&self) -> &[String] {
        &self.labels
    }
}

/// A node in the device tree.
///
/// The node can have labels, contain properties, and contain other nodes. The
/// node also tracks it's own offset in the buffer that it was parsed from.
#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Node {
    /// A deleted node, made from a `/delete-node/` statement.
    Deleted {
        /// The name of the node, containing either as a ref or the final part
        /// of the full path
        name: NodeName,

        /// The offset in bytes that this `Node` was found at within the buffer
        /// that the containing tree was parsed from.
        offset: usize
    },
    /// A regular node.
    Existing {
        /// The name of the node, containing either as a ref or the final part
        /// of the full path
        name: NodeName,

        /// The properties contained within this node. Stored in a hashmap with
        /// the key being the name of the `Property` and the value the Property
        /// its self.
        proplist: HashMap<String, Property>,

        /// The child nodes contained within this node. Stored in a hashmap with
        /// the key being the name of the child `Node` and the value the child
        /// `Node` its self.
        children: HashMap<String, Node>,

        // fullpath: Option<PathBuf>,
        // length to the # part of node_name@#
        // basenamelen: usize,
        //
        // phandle: u32,
        // addr_cells: i32,
        // size_cells: i32,

        /// The labels that refer to this node.
        labels: Vec<String>,

        /// The offset in bytes that this `Node` was found at within the buffer
        /// that the containing tree was parsed from.
        offset: usize,
    },
}

impl Node {
    /// Convenience function to get the `NodeName` no matter what form the
    /// `Node`is in.
    pub fn name(&self) -> &NodeName {
        match *self {
            Node::Deleted { ref name, .. } |
            Node::Existing { ref name, .. } => name,
        }
    }

    /// Merge one `Node` into another. If a property exists in both `Node`s the
    /// value in the `other` `Node` will be kept. This merge is also applied to
    /// all child nodes, recursively.
    fn merge(&mut self, other: &Node) {
        match (self, other) {
            (&mut Node::Existing { proplist: ref mut s_props,
                                   children: ref mut s_childs,
                                   labels: ref mut s_labels,
                                   .. },
             &Node::Existing { proplist: ref o_props,
                               children: ref o_childs,
                               labels: ref o_labels,
                               .. }) => {
                // merge props
                s_props.extend(o_props.iter().map(|(k, v)| (k.to_owned(), v.to_owned())));
                // merge nodes
                for (name, node) in o_childs {
                    match node {
                        &Node::Deleted { .. } => { s_childs.remove(name).expect(&format!("Deleted non-existent node {}", name)); } // FIXME: should there always be a deletable node when deleting?
                        &Node::Existing { .. } => {
                            let entry = s_childs.entry(name.to_owned());
                            if let Entry::Occupied(mut e) = entry {
                                e.get_mut().merge(node);
                            } else {
                                entry.or_insert_with(|| node.clone());
                            }
                        }
                    }
                }
                // merge labels
                s_labels.extend(o_labels.iter().map(|s| s.to_owned()));
                s_labels.sort();
                s_labels.dedup();
            }
            _ => unreachable!()
        }
    }
}

impl Labeled for Node {
    fn add_label(&mut self, label: &str) -> Result<(), ()> {
        match *self {
            Node::Deleted { .. } => Err(()),
            Node::Existing { ref mut labels, .. } => {
                let label = label.to_owned();
                if labels.contains(&label) {
                    labels.push(label);
                }

                Ok(())
            }
        }
    }

    fn get_labels(&self) -> &[String] {
        match *self {
            Node::Deleted { .. } => &[],
            Node::Existing { ref labels, .. } => labels,
        }
    }
}

impl Offset for Node {
    fn get_offset(&self) -> usize {
        match *self {
            Node::Deleted { offset, .. } |
            Node::Existing { offset, .. } => offset,
        }
    }
}

impl fmt::Display for Node {
    // TODO: labels - issue 3
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Node::Deleted { ref name, .. } => write!(f, "// Node {} deleted", name)?,
            Node::Existing { ref name, ref proplist, ref children, .. } => {
                writeln!(f, "{} {{", name)?;
                for prop in proplist.values() {
                    writeln!(f, "    {}", prop)?;
                }
                for node in children.values() {
                    match *node {
                        Node::Deleted { ref name, .. } => {
                            writeln!(f, "    // Node {} deleted", name)?
                        }
                        Node::Existing { ref name, .. } => writeln!(f, "    {} {{ ... }}", name)?,
                    }
                }
                write!(f, "}}")?;
            }
        }

        Ok(())
    }
}

/// A name of a node, either as a reference or a full path.
#[derive(PartialEq, Eq, Debug, Clone)]
pub enum NodeName {
    /// A reference to a previously defined label of a node.
    Ref(String),
    /// The true name of a node. If appended to the parent node's path, this
    /// node's path will be created.
    Full(String),
}

impl fmt::Display for NodeName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            NodeName::Ref(ref name) |
            NodeName::Full(ref name) => write!(f, "{}", name),
        }
    }
}

impl NodeName {
    /// Extracts the name as a string slice.
    ///
    /// This does no conversion so the slice could be a reference or a full
    /// name.
    pub fn as_str(&self) -> &str {
        match *self {
            NodeName::Ref(ref name) |
            NodeName::Full(ref name) => name,
        }
    }
}

/// A property of a node.
///
/// The property tracks it's own offset in the buffer that it was parsed from.
#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Property {
    /// A deleted node, made from a `/delete-property/` statement.
    Deleted {
        /// The full name of the property.
        name: String,
        /// The offset in bytes that this `Property` was found at within the
        /// buffer that the containing tree was parsed from.
        offset: usize
    },
    /// A normal property. Unlike `Node`s, a `Property`'s name will never be a
    /// reference. If `val` is `None` then the property was in the form
    /// ```ignore
    /// prop;
    /// ```
    /// other wise the property was in the form
    /// ```ignore
    /// prop = data;
    /// ```
    Existing {
        /// The full name of the property.
        name: String,
        /// The values assigned to the property. `None` if the property is in a
        /// marker form with no values.
        val: Option<Vec<Data>>,
        /// The labels that refer to this property.
        labels: Vec<String>,
        /// The offset in bytes that this `Property` was found at within the
        /// buffer that the containing tree was parsed from.
        offset: usize,
    },
}

impl Property {
    /// Convenience function to get the name no matter what form the
    /// `Property`is in.
    pub fn name(&self) -> &str {
        match *self {
           Property::Deleted{ref name, ..} |
           Property::Existing{ref name, ..} => name
        }
    }
}

impl Labeled for Property {
    fn add_label(&mut self, label: &str) -> Result<(), ()> {
        match *self {
            Property::Deleted { .. } => Err(()),
            Property::Existing { ref mut labels, .. } => {
                let label = label.to_owned();
                if labels.contains(&label) {
                    labels.push(label);
                }
                Ok(())
            }
        }
    }

    fn get_labels(&self) -> &[String] {
        match *self {
            Property::Deleted { .. } => &[],
            Property::Existing { ref labels, .. } => labels,
        }
    }
}

impl Offset for Property {
    fn get_offset(&self) -> usize {
        match *self {
            Property::Deleted { offset, .. } |
            Property::Existing { offset, .. } => offset,
        }
    }
}

impl fmt::Display for Property {
    // TODO: labels - issue 3
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Property::Deleted { ref name, .. } => write!(f, "// Property {} deleted", name)?,
            Property::Existing { ref name, ref val, .. } => {
                write!(f, "{}", name)?;
                if let Some(ref data) = *val {
                    if !data.is_empty() {
                        let mut iter = data.iter();
                        write!(f, " = {}", iter.next().unwrap())?;
                        for d in iter {
                            write!(f, ", {}", d)?;
                        }
                    }
                }
                write!(f, ";")?;
            }
        }

        Ok(())
    }
}

/// Data that properties might contain.
#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Data {
    /// A reference to a labeled object. Could be a node, a property, or data.
    /// May contain the phandle number of the referenced object.
    Reference(String, Option<u64>),
    /// An ASCII string. See `parser::escape_c_string` to see the few
    /// limitations placed on the string.
    String(String),
    /// A list of cells. The preceding `usize` to the `Vec` of cells is the number of bits used to
    /// represent each cell. If not set with a preceding `/bits/` statement, it defaults to 32 bits.
    Cells(usize, Vec<Cell>),
    /// An array of bytes.
    ByteArray(Vec<u8>),
}

impl fmt::Display for Data {
    // TODO: labels - issue 3 - issue 6
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Data::Reference(ref r, _) => write!(f, "&{}", r)?,
            Data::String(ref s) => write!(f, "{}", s)?,
            Data::Cells(bits, ref cells) => {
                if bits != 32 {
                    write!(f, "/bits/ {}", bits)?;
                }
                write!(f, "<")?;
                if !cells.is_empty() {
                    let mut iter = cells.iter();
                    write!(f, "{}", iter.next().unwrap())?;
                    for c in iter {
                        write!(f, "{}", c)?;
                    }
                }
                write!(f, ">")?;
            }
            Data::ByteArray(ref arr) => {
                write!(f, "[ ")?;
                if !arr.is_empty() {
                    let mut iter = arr.iter();
                    write!(f, "{:02X}", iter.next().unwrap())?;
                    for d in iter {
                        write!(f, " {:02X}", d)?;
                    }
                }
                write!(f, " ]")?;
            }
        }

        Ok(())
    }
}

/// A cell that can hold either a number or a reference to another object.
#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Cell {
    /// A number represented in a certain number of bits. The number of bits is defined in the
    /// `Data::Cells` holding this `Cell`.
    Num(u64),
    /// A reference to a labeled object. Could be a node, a property, or data.
    /// May contain the phandle number of the referenced object.
    Ref(String, Option<u64>),
}

impl fmt::Display for Cell {
    // TODO: labels - issue 3 - issue 6
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Cell::Num(i) => write!(f, "{}", i)?,
            Cell::Ref(ref s, _) => write!(f, "&{}", s)?,
        }

        Ok(())
    }
}

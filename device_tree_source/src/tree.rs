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

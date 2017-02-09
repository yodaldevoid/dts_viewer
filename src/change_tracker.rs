use std::collections::HashMap;
use std::path::{ Path, PathBuf };

use dts_parser::{ BootInfo, Node, Property };

#[derive(PartialEq, Eq, Hash, Debug)]
enum Element<'a> {
    Node(&'a Node),
    Prop(&'a Property),
}

#[derive(Debug)]
pub struct AliasStore<'a> {
    paths: HashMap<PathBuf, Vec<Element<'a>>>,
    aliases: HashMap<&'a str, PathBuf>,
}

// TODO: somehow keep track of deleted aliases so they can be searched for later
//       while not being used for path lookup during change parsing
pub fn create_alias_store<'a>(boot_info: &'a BootInfo, ammends: &'a [Node]) -> AliasStore<'a> {
    let mut path_map = HashMap::new();
    let mut alias_map = HashMap::new();
    create_alias_store_internal(Path::new("/"), &boot_info.root, &mut path_map, &mut alias_map);
    for node in ammends {
        match *node {
            Node::Existing { ref name, .. } => {
                if name == "/" {
                    create_alias_store_internal(Path::new("/"),
                                                node,
                                                &mut path_map,
                                                &mut alias_map);
                } else if alias_map.contains_key(name.as_str()) {
                    create_alias_store_internal(&alias_map[name.as_str()].clone(),
                                                node,
                                                &mut path_map,
                                                &mut alias_map);
                } else {
                    unimplemented!();
                }
            }
            Node::Deleted(_) => unreachable!(),
        }
    }
    AliasStore { paths: path_map, aliases: alias_map }
}

fn create_alias_store_internal<'a>(path: &Path,
                                   node: &'a Node,
                                   path_map: &mut HashMap<PathBuf, Vec<Element<'a>>>,
                                   alias_map: &mut HashMap<&'a str, PathBuf>) {
    match *node {
        Node::Deleted(ref name) => {
            let node_path = path.join(name);
            let mut aliases: Vec<&str> = Vec::new();
            for (alias, path) in alias_map.iter() {
                if path.starts_with(&node_path) {
                    aliases.push(alias);
                }
            }
            for alias in &aliases {
                alias_map.remove(alias);
            }

            path_map.entry(path.join(name)).or_insert_with(Vec::new).push(Element::Node(node));
            // TODO: delete non-deleted children
        }
        Node::Existing { ref name, ref proplist, ref children, ref labels } => {
            let node_path = path.join(name);

            for label in labels {
                if !alias_map.contains_key(label.as_str()) {
                    alias_map.insert(label, node_path.clone());
                } else if *alias_map.get(label.as_str()).unwrap() != node_path {
                    // TODO: error, duplicate labels
                    panic!("Duplicate label \"{}\" at different paths", label);
                }
            }

            for prop in proplist {
                match *prop {
                    Property::Deleted(ref name) => {
                        let label_path = node_path.join(name);
                        let mut aliases: Vec<&str> = Vec::new();
                        for (alias, path) in alias_map.iter() {
                            if path == &label_path {
                                aliases.push(alias);
                            }
                        }
                        for alias in &aliases {
                            alias_map.remove(alias);
                        }

                        path_map.entry(node_path.join(name)).or_insert_with(Vec::new).push(Element::Prop(prop));
                    },
                    Property::Existing { ref name, ref labels, .. } => {
                        let label_path = node_path.join(name);

                        for label in labels {
                            if !alias_map.contains_key(label.as_str()) {
                                alias_map.insert(label, label_path.clone());
                            } else {
                                // TODO: error, duplicate labels
                                unimplemented!()
                            }
                        }

                        path_map.entry(label_path).or_insert_with(Vec::new).push(Element::Prop(prop));
                    },
                }
            }

            for node in children {
                create_alias_store_internal(&node_path, node, path_map, alias_map);
            }

            path_map.entry(node_path).or_insert_with(Vec::new).push(Element::Node(node));
        }
    }
}

use libc::*;
use std::ptr::null_mut;
use std::ffi::CStr;

use std::path::{Path, PathBuf};

trait Labeled {
	fn add_label(&mut self, label: &str);
}

pub struct BootInfo {
	boot_cpuid: u32,
	root: DeviceNode,
	reserve_info: Vec<ReserveInfo>,
}

impl BootInfo {
	fn check(&mut self) -> Result<(), String> {
		unimplemented!()
	}

	fn fill_fullpaths(&mut self) {
		unimplemented!()
	}
}

pub struct ReserveInfo {
	entry: ReserveEntry,
	labels: Vec<String>,
}

struct ReserveEntry {
	address: u64,
	size: u64,
}

impl Labeled for ReserveInfo {
	fn add_label(&mut self, label: &str) {
		let label = label.to_string();
		if !self.labels.contains(&label) {
			self.labels.push(label);
		}
	}
}

pub struct DeviceNode {
	deleted: bool,
	name: Option<String>,
	proplist: Vec<Property>,
	children: Vec<DeviceNode>,

	//struct node *parent;
	//struct node *next_sibling;

	fullpath: Option<PathBuf>, //should be Path
	//length to the # part of node_name@#
	basenamelen: usize,

	phandle: u32,
	addr_cells: i32,
	size_cells: i32,

	labels: Vec<String>,
}

impl Labeled for DeviceNode {
	fn add_label(&mut self, label: &str) {
		let label = label.to_string();
		if !self.labels.contains(&label) {
			self.labels.push(label);
		}
	}
}

impl DeviceNode {
	fn delete(&mut self) {
		self.deleted = true;
		for node in self.children.iter_mut() {
			node.delete();
		}
		for prop in self.proplist.iter_mut() {
			prop.delete();
		}
	}

	fn merge(&mut self, other: DeviceNode) {
		let mut other = other;

		self.deleted = false;

		self.labels.append(&mut other.labels);
		self.labels.sort();
		self.labels.dedup();

		'props: for other_prop in other.proplist {
			for prop in self.proplist.iter_mut() {
				if prop.name == other_prop.name {
					if other_prop.deleted {
						prop.delete();
					} else {
						let mut other_prop = other_prop;
						prop.labels.append(&mut other_prop.labels);
						prop.labels.sort();
						prop.labels.dedup();

						prop.deleted = false;
						prop.val = other_prop.val;
					}

					continue 'props;
				}
			}

			self.proplist.push(other_prop);
		}

		'nodes: for other_node in other.children {
			for node in self.children.iter_mut() {
				if node.name == other_node.name {
					if other_node.deleted {
						node.delete();
					} else {
						node.merge(other_node);
					}

					continue 'nodes;
				}
			}

			self.children.push(other_node);
		}
	}
}

pub struct Property {
	deleted: bool,
	name: String,
	val: Data,
	labels: Vec<String>,
}

impl Labeled for Property {
	fn add_label(&mut self, label: &str) {
		let label = label.to_string();
		if !self.labels.contains(&label) {
			self.labels.push(label);
		}
	}
}

impl Property {
	fn delete(&mut self) {
		self.deleted = true;
	}
}

pub struct Data {
	val: Vec<u8>,
	markers: Vec<Marker>,
}

impl Data {
	fn empty_data() -> Data {
		Data {
			val: Vec::new(),
			markers: Vec::new(),
		}
	}
}

enum Marker {
	RefHandle {
		offset: i32,
		reference: String,
	},
	RefPath {
		offset: i32,
		reference: String,
	},
	Label {
		offset: i32,
		reference: String,
	},
}

#[repr(C)]
enum MarkerType {
	REF_PHANDLE,
	REF_PATH,
	LABEL,
}

extern "C" {
	fn yyparse() -> c_int; //0 = success, 1 = invalid input, 2 = memory exhaustion
}

#[allow(non_upper_case_globals)]
pub static mut treesource_error: bool = false;
#[allow(non_upper_case_globals)]
pub static mut the_boot_info: *mut BootInfo = null_mut();

#[no_mangle]
pub extern "C" fn data_free(d: *mut Data) {
	if !d.is_null() {
		let data = unsafe { Box::from_raw(d) };
	}
}

/*
#[no_mangle]
pub extern "C" fn data_copy_escape_string(s: *const c_char, len: c_int) -> *mut Data;

pub extern "C" fn data_copy_file(FILE *f, size_t len) -> *mut Data;
#[no_mangle]
pub extern "C" fn data_merge(d1: *mut Data, d2: *const Data) -> *mut Data;
#[no_mangle]
pub extern "C" fn data_append_integer(d: *mut Data, word: c_uintlong, bits: c_int) -> *mut Data;
#[no_mangle]
pub extern "C" fn data_append_byte(d: *mut Data, byte: c_uchar) -> *mut Data;
#[no_mangle]
pub extern "C" fn data_add_marker(d: *mut Data, enum markertype type,
						reference: *const c_char) -> *mut Data;
*/
#[no_mangle]
pub extern "C" fn memreserve_add_label(info: *mut ReserveInfo, label: *const c_char) {
	if info.is_null() || label.is_null() {
		// abort
	}

	let info = unsafe { &mut *info };
	let label = unsafe { CStr::from_ptr(label) };

	info.add_label(label.to_str().unwrap()); //TODO: error checking
}

#[no_mangle]
pub extern "C" fn node_add_label(node: *mut DeviceNode, label: *const c_char) {
	if node.is_null() || label.is_null() {
		// abort
	}

	let node = unsafe { &mut *node };
	let label = unsafe { CStr::from_ptr(label) };

	node.add_label(label.to_str().unwrap()); //TODO: error checking
}

#[no_mangle]
pub extern "C" fn vec_node_add_label(nodes: *mut Vec<DeviceNode>, label: *mut c_char) {
	if nodes.is_null() || label.is_null() {
		// abort
	}

	let nodes = unsafe { &mut *nodes };
	let label = unsafe { CStr::from_ptr(label) };

	if let Some(node) = nodes.last_mut() {
		node.add_label(label.to_str().unwrap()); //TODO: error checking
	} else {
		// abort
	}
}

#[no_mangle]
pub extern "C" fn property_add_label(prop: *mut Property, label: *const c_char) {
	if prop.is_null() || label.is_null() {
		// abort
	}

	let prop = unsafe { &mut *prop };
	let label = unsafe { CStr::from_ptr(label) };

	prop.add_label(label.to_str().unwrap()); //TODO: error checking
}

#[no_mangle]
pub extern "C" fn build_property(name: *const c_char, val: *mut Data) -> *mut Property {
	if name.is_null() || val.is_null() {
		// abort
	}

	let name = unsafe { CStr::from_ptr(name) };
	let val = unsafe { Box::from_raw(val) };

	Box::into_raw(
		box Property {
			deleted: false,
			name: name.to_str().unwrap().to_string(), //TODO: check for error
			val: *val,
			labels: Vec::new(),
	})
}

#[no_mangle]
pub extern "C" fn build_property_delete(name: *const c_char) -> *mut Property {
	if name.is_null() {
		// abort
	}

	let name = unsafe { CStr::from_ptr(name) };

	Box::into_raw(
		box Property {
			deleted: true,
			name: name.to_str().unwrap().to_string(), //TODO: check for error
			val: Data::empty_data(),
			labels: Vec::new(),
	})
}

#[no_mangle]
pub extern "C" fn chain_property(first: *mut Property,
						list: *mut Vec<Property>) -> *mut Vec<Property> {
	if first.is_null() {
		return null_mut();
	}

	let first = unsafe { Box::from_raw(first) };

	let list = unsafe { 
		&mut * if list.is_null() {
			Box::into_raw(box Vec::new())
		} else {
			list
		}
	};

	list.push(*first);
	list as *mut Vec<Property>
}

#[no_mangle]
pub extern "C" fn build_node(proplist: *mut Vec<Property>,
						children: *mut Vec<DeviceNode>) -> *mut DeviceNode {
	if proplist.is_null() || children.is_null() {
		return null_mut();
	}

	let proplist = unsafe { Box::from_raw(proplist) };
	let children = unsafe { Box::from_raw(children) };

	Box::into_raw(
		box DeviceNode {
			deleted: false,
			name: None,
			proplist: *proplist,
			children: *children,

			fullpath: None,
			basenamelen: 0,

			phandle: 0,
			addr_cells: 0,
			size_cells: 0,

			labels: Vec::new(),
	})
}

#[no_mangle]
pub extern "C" fn build_node_delete() -> *mut DeviceNode {
	Box::into_raw(
		box DeviceNode {
			deleted: true,
			name: None,
			proplist: Vec::new(),
			children: Vec::new(),

			fullpath: None, //should be Path
			//length to the # part of node_name@#
			basenamelen: 0,

			phandle: 0,
			addr_cells: 0,
			size_cells: 0,

			labels: Vec::new(),
	})
}

#[no_mangle]
pub extern "C" fn name_node(node: *mut DeviceNode, name: *const c_char) -> *mut DeviceNode {
	if node.is_null() || name.is_null() {
		// abort
	}

	let node = unsafe { &mut *node };
	let name = unsafe { CStr::from_ptr(name) };

	node.name = Some(name.to_str().unwrap().to_string());

	node as *mut DeviceNode
}

#[no_mangle]
pub extern "C" fn chain_node(first: *mut DeviceNode, list: *mut Vec<DeviceNode>) -> *mut Vec<DeviceNode> {
	if first.is_null() {
		return null_mut();
	}

	let first = unsafe { Box::from_raw(first) };

	let list = unsafe { 
		&mut * if list.is_null() {
			Box::into_raw(box Vec::new())
		} else {
			list
		}
	};

	list.push(*first);
	list as *mut Vec<DeviceNode>
}

#[no_mangle]
pub extern "C" fn merge_nodes(old_node: *mut DeviceNode, new_node: *mut DeviceNode) -> *mut DeviceNode {
	if old_node.is_null() || new_node.is_null() {
		// abort
	}

	let old_node = unsafe { &mut *old_node };
	let new_node = unsafe { Box::from_raw(new_node) };
	old_node.merge(*new_node);
	old_node as *mut DeviceNode
}

#[no_mangle]
pub extern "C" fn delete_node(node: *mut DeviceNode) {
	if node.is_null() {
		// abort
	}

	let node = unsafe { &mut *node };
	node.delete()	
}
/*
#[no_mangle]
pub extern "C" fn get_node_by_ref(tree: *const DeviceNode, reference: *const c_char) -> *mut DeviceNode;
#[no_mangle]
pub extern "C" fn guess_boot_cpuid(tree: *const DeviceNode) -> c_uint;
#[no_mangle]
pub extern "C" fn build_reserve_entry(start: c_uintlong, len: c_uintlong) -> *mut ReserveInfo;
#[no_mangle]
pub extern "C" fn chain_reserve_entry(first: *const ReserveInfo,
						list: *mut Vec<ReserveInfo>) -> *mut ReserveInfo;
*/
#[no_mangle]
pub extern "C" fn build_boot_info(reserve_list: *mut Vec<ReserveInfo>,
						tree: *mut DeviceNode, boot_cpuid_phys: c_uint) -> *mut BootInfo {
	if reserve_list.is_null() || tree.is_null() {
		return null_mut();
	}

	let reserve_info = unsafe { Box::from_raw(reserve_list) };
	let root = unsafe { Box::from_raw(tree) };

	Box::into_raw(
		box BootInfo {
			boot_cpuid: boot_cpuid_phys as u32,
			root: *root,
			reserve_info: *reserve_info,
	})
}
/*
#[no_mangle]
pub extern "C" fn xstrdup(s: *const c_char) -> *mut c_char;
*/

pub fn dt_from_source(path: &Path) -> Result<&mut BootInfo, String> {
	unsafe {
		the_boot_info = null_mut();
		treesource_error = false;
	}

	if unsafe{ yyparse() != 0 } {
		Err(String::from("Unable to parse input tree"))
	} else if unsafe { treesource_error } {
		Err(String::from("Syntax error parsing input tree"))
	} else {
		//TODO: check for null
		let boot_info = unsafe { the_boot_info.as_mut() };
		if let Some(boot_info) = boot_info {
			boot_info.fill_fullpaths();
			boot_info.check();
			Ok(boot_info)
		} else {
			Err(String::from("Boot info is null"))
		}
	}
}

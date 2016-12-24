use libc::*;
use byteorder::{BigEndian, ReadBytesExt, WriteBytesExt};
use std::ptr::{self, null_mut};
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

	fullpath: Option<PathBuf>,
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
		self.labels.clear();
		for node in &mut self.children {
			node.delete();
		}
		for prop in &mut self.proplist {
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
			for prop in &mut self.proplist {
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
			for node in &mut self.children {
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

	fn get_node_by_label(&self, reference: &str) -> Option<&DeviceNode> {
		if self.labels.contains(&reference.to_string()) {
			Some(self)
		} else {
			self.children.iter().flat_map(|node| node.get_node_by_label(reference)).next()
			/*
			for node in self.children.iter() {
				if let Some(node) = node.get_node_by_label(reference) {
					return Some(node);
				}
			}

			None
			*/
		}
	}

	fn get_node_by_label_mut(&mut self, reference: &str) -> Option<&mut DeviceNode> {
		if self.labels.contains(&reference.to_string()) {
			Some(self)
		} else {
			self.children.iter_mut().flat_map(|node| node.get_node_by_label_mut(reference)).next()
			/*
			for node in self.children.iter_mut() {
				if let Some(node) = node.get_node_by_label_mut(reference) {
					return Some(node);
				}
			}

			None
			*/
		}
	}

	fn get_node_by_path(&self, path: &str) -> Option<&DeviceNode> {
		if let Some(index) = path.find('/') {
			let (_, ending) = path.split_at(index);
			let ending = ending.trim_left_matches('/');
			ending.find('/')
				.and_then(|index| {
					let (start, _) = path.split_at(index);
					self.children.iter()
						.find(|node| 
							if let Some(ref name) = node.name {
								start == name
						} else {
							false
						}
					)
				})
				.and_then(|child| child.get_node_by_path(ending))
		} else {
			if let Some(ref name) = self.name {
				if name == path {
					return Some(self);
				}
			}

			None
		}
	}

	fn get_node_by_path_mut(&mut self, path: &str) -> Option<&mut DeviceNode> {
		if let Some(index) = path.find('/') {
			let (_, ending) = path.split_at(index);
			let ending = ending.trim_left_matches('/');
			let start = ending.find('/').and_then(|index| {
					let (start, _) = path.split_at(index);
					Some(start)
				});
			if let Some(start) = start {
				self.children.iter().position(|node| 
					if let Some(ref name) = node.name {
						start == name
					} else {
						false
					})
					.and_then(move |index| self.children[index].get_node_by_path_mut(ending))
			} else {
				None
			}
		} else {
			let is_self = match self.name {
				Some(ref name) => name == path,
				None => false
			};

			if is_self {
				Some(self)
			} else {
				None
			}
		}
	}

	fn get_property(&self, name: &str) -> Option<&Property> {
		self.proplist.iter().find(|prop| &prop.name == name)
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
		self.labels.clear();
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

	unsafe fn from_ptr(d: *mut Data) -> &'static mut Data {
		if d.is_null() {
			&mut *Box::into_raw(Box::new(Data::empty_data()))
		} else {
			&mut *d
		}
	}

	fn merge(&mut self, other: Data) {
		let mut other = other;
		let len = self.markers.len();
		for marker in other.markers {
			let new = match marker {
				Marker::RefPHandle{offset, reference} =>
					Marker::RefPHandle {offset: offset + len, reference: reference},
				Marker::RefPath{offset, reference} =>
					Marker::RefPath {offset: offset + len, reference: reference},
				Marker::Label{offset, reference} =>
					Marker::Label {offset: offset + len, reference: reference},
			};

			self.markers.push(new);
		}
		self.val.append(&mut other.val);
	}
}

enum Marker {
	RefPHandle {
		offset: usize,
		reference: String,
	},
	RefPath {
		offset: usize,
		reference: String,
	},
	Label {
		offset: usize,
		reference: String,
	},
}

#[repr(C)]
pub enum MarkerType {
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
pub static mut the_boot_info: Option<&'static mut BootInfo> = None ;

#[no_mangle]
#[allow(unused_variables)]
pub unsafe extern "C" fn data_free(d: *mut Data) {
	if !d.is_null() {
		let data = Box::from_raw(d);
	}
}

#[no_mangle]
pub extern "C" fn data_copy_escape_string(s: *const c_char, len: c_int) -> *mut Data {
	unimplemented!()
}
/*
pub extern "C" fn data_copy_file(FILE *f, size_t len) -> *mut Data;
*/
#[no_mangle]
pub unsafe extern "C" fn data_merge(prefix: *mut Data, data: *mut Data) -> *mut Data {
	if prefix.is_null() || data.is_null() {
		return data; // null prefix or null data, nothing to merge
	}

	let prefix = &mut *prefix;
	let data = Box::from_raw(data);

	prefix.merge(*data);
	prefix as *mut Data
}

#[no_mangle]
pub unsafe extern "C" fn data_append_integer(d: *mut Data, word: c_ulong, bits: c_int) -> *mut Data{
	if d.is_null()  {
		return null_mut();
	}

	let d = &mut *d;

	let result = match bits {
		8 => d.val.write_u8(word as u8),
		16 => d.val.write_u16::<BigEndian>(word as u16),
		32 => d.val.write_u32::<BigEndian>(word as u32),
		64 => d.val.write_u64::<BigEndian>(word),
		_ => return null_mut(),
	};

	match result {
		Ok(_) => d as *mut Data,
		Err(_) => null_mut(),
	}
}

#[no_mangle]
pub unsafe extern "C" fn data_append_byte(d: *mut Data, byte: c_uchar) -> *mut Data {
	data_append_integer(d, byte as c_ulong, 8)
}

#[no_mangle]
pub unsafe extern "C" fn data_add_marker(d: *mut Data, 
								m_type: MarkerType,
								reference: *const c_char)
								-> *mut Data {
	if reference.is_null() {
		return null_mut();
	}

	let d = Data::from_ptr(d);
	let reference = CStr::from_ptr(reference);

	let m = match m_type {
		MarkerType::REF_PHANDLE => Marker::RefPHandle {
				offset: d.val.len(),
				reference: reference.to_str().unwrap().to_string(),
			},
		MarkerType::REF_PATH => Marker::RefPath {
				offset: d.val.len(),
				reference: reference.to_str().unwrap().to_string(),
			},
		MarkerType::LABEL => Marker::Label {
				offset: d.val.len(),
				reference: reference.to_str().unwrap().to_string(),
			},
	};

	d.markers.push(m);

	d as *mut Data
}

#[no_mangle]
pub unsafe extern "C" fn memreserve_add_label(info: *mut ReserveInfo, label: *const c_char) {
	if !info.is_null() && !label.is_null() {
		let info = &mut *info;
		let label = CStr::from_ptr(label);

		info.add_label(label.to_str().unwrap()); //TODO: error checking
	}
}

#[no_mangle]
pub unsafe extern "C" fn node_add_label(node: *mut DeviceNode, label: *const c_char) {
	if !node.is_null() && !label.is_null() {
		let node = &mut *node;
		let label = CStr::from_ptr(label);

		node.add_label(label.to_str().unwrap()); //TODO: error checking
	}
}

#[no_mangle]
pub unsafe extern "C" fn vec_node_add_label(nodes: *mut Vec<DeviceNode>, label: *mut c_char) {
	if !nodes.is_null() && !label.is_null() {
		let nodes = &mut *nodes;
		let label = CStr::from_ptr(label);

		if let Some(node) = nodes.last_mut() {
			node.add_label(label.to_str().unwrap()); //TODO: error checking
		}
	}
}

#[no_mangle]
pub unsafe extern "C" fn property_add_label(prop: *mut Property, label: *const c_char) {
	if !prop.is_null() && !label.is_null() {
		let prop = &mut *prop;
		let label = CStr::from_ptr(label);

		prop.add_label(label.to_str().unwrap()); //TODO: error checking
	}
}

#[no_mangle]
pub unsafe extern "C" fn build_property(name: *const c_char, val: *mut Data) -> *mut Property {
	if name.is_null() {
		return null_mut();
	}

	let name = CStr::from_ptr(name);

	Box::into_raw(Box::new(
		Property {
			deleted: false,
			name: name.to_str().unwrap().to_string(), //TODO: check for error
			val: if val.is_null() {
					Data::empty_data()
				} else {
					*Box::from_raw(val)
				},
			labels: Vec::new(),
	}))
}

#[no_mangle]
pub unsafe extern "C" fn build_property_delete(name: *const c_char) -> *mut Property {
	if name.is_null() {
		return null_mut();
	}

	let name = CStr::from_ptr(name);

	Box::into_raw(Box::new(
		Property {
			deleted: true,
			name: name.to_str().unwrap().to_string(), //TODO: check for error
			val: Data::empty_data(),
			labels: Vec::new(),
	}))
}

#[no_mangle]
pub unsafe extern "C" fn chain_property(first: *mut Property,
								list: *mut Vec<Property>)
								-> *mut Vec<Property> {
	if first.is_null() {
		return null_mut();
	}

	let first = Box::from_raw(first);

	let list = &mut * if list.is_null() {
		Box::into_raw(Box::new(Vec::new()))
	} else {
		list
	};

	list.push(*first);
	list as *mut Vec<Property>
}

#[no_mangle]
pub unsafe extern "C" fn build_node(proplist: *mut Vec<Property>,
							children: *mut Vec<DeviceNode>)
							-> *mut DeviceNode {
	if proplist.is_null() || children.is_null() {
		return null_mut();
	}

	let proplist = Box::from_raw(proplist);
	let children = Box::from_raw(children);

	Box::into_raw(Box::new(
		DeviceNode {
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
	}))
}

#[no_mangle]
pub extern "C" fn build_node_delete() -> *mut DeviceNode {
	Box::into_raw(Box::new(
		DeviceNode {
			deleted: true,
			name: None,
			proplist: Vec::new(),
			children: Vec::new(),

			fullpath: None,
			basenamelen: 0,

			phandle: 0,
			addr_cells: 0,
			size_cells: 0,

			labels: Vec::new(),
	}))
}

#[no_mangle]
pub unsafe extern "C" fn name_node(node: *mut DeviceNode, name: *const c_char) -> *mut DeviceNode {
	if node.is_null() || name.is_null() {
		return null_mut();
	}

	let node = &mut *node;
	let name = CStr::from_ptr(name);

	node.name = Some(name.to_str().unwrap().to_string());

	node as *mut DeviceNode
}

#[no_mangle]
pub unsafe extern "C" fn chain_node(first: *mut DeviceNode,
							list: *mut Vec<DeviceNode>)
							-> *mut Vec<DeviceNode> {
	if first.is_null() {
		return null_mut();
	}

	let first = Box::from_raw(first);

	let list = &mut * if list.is_null() {
		Box::into_raw(Box::new(Vec::new()))
	} else {
		list
	};

	list.push(*first);
	list as *mut Vec<DeviceNode>
}

#[no_mangle]
pub unsafe extern "C" fn merge_nodes(old_node: *mut DeviceNode,
							new_node: *mut DeviceNode)
							-> *mut DeviceNode {
	if old_node.is_null() || new_node.is_null() {
		return null_mut();
	}

	let old_node = &mut *old_node;
	let new_node = Box::from_raw(new_node);
	old_node.merge(*new_node);
	old_node as *mut DeviceNode
}

#[no_mangle]
pub unsafe extern "C" fn delete_node(node: *mut DeviceNode) {
	if !node.is_null() {
		let node = &mut *node;
		node.delete();
	}
}

#[no_mangle]
pub unsafe extern "C" fn get_node_by_ref(tree: *mut DeviceNode,
								reference: *const c_char)
								-> *mut DeviceNode {
	if tree.is_null() || reference.is_null() {
		return null_mut();
	}

	let tree = &mut *tree;
	let reference = CStr::from_ptr(reference).to_str().unwrap();

	if reference.starts_with('/') {
		match tree.get_node_by_path_mut(reference) {
			Some(node) => node as *mut DeviceNode,
			None => null_mut(),
		}
	} else {
		match tree.get_node_by_label_mut(reference) {
			Some(node) => node as *mut DeviceNode,
			None => null_mut(),
		}
	}
}

#[no_mangle]
pub unsafe extern "C" fn guess_boot_cpuid(tree: *const DeviceNode) -> c_uint {
	if tree.is_null() {
		// abort
	}

	let tree = &*tree;

	tree.get_node_by_path("/cpus")
		.and_then(|cpus| cpus.children.first())
		.and_then(|cpu| cpu.get_property("reg"))
		.and_then(|reg| reg.val.val.as_slice().read_u32::<BigEndian>().ok())
		.unwrap_or(0)
}

#[no_mangle]
pub extern "C" fn build_reserve_entry(start: c_ulong, len: c_ulong) -> *mut ReserveInfo {
	Box::into_raw(Box::new(
		ReserveInfo {
			entry: ReserveEntry {
				address: start as u64,
				size: len as u64,
			},
			labels: Vec::new(),
	}))
}

#[no_mangle]
pub unsafe extern "C" fn chain_reserve_entry(first: *mut ReserveInfo,
									list: *mut Vec<ReserveInfo>)
									-> *mut Vec<ReserveInfo> {
	if first.is_null() {
		return null_mut();
	}

	let first = Box::from_raw(first);

	let list = &mut * if list.is_null() {
		Box::into_raw(Box::new(Vec::new()))
	} else {
		list
	};

	list.push(*first);
	list as *mut Vec<ReserveInfo>
}

#[no_mangle]
pub unsafe extern "C" fn build_boot_info(reserve_list: *mut Vec<ReserveInfo>,
								tree: *mut DeviceNode,
								boot_cpuid_phys: c_uint)
								-> *mut BootInfo {
	if reserve_list.is_null() || tree.is_null() {
		return null_mut();
	}

	let reserve_info = Box::from_raw(reserve_list);
	let root = Box::from_raw(tree);

	Box::into_raw(Box::new(
		BootInfo {
			boot_cpuid: boot_cpuid_phys as u32,
			root: *root,
			reserve_info: *reserve_info,
	}))
}
/*
#[no_mangle]
pub extern "C" fn xstrdup(s: *const c_char) -> *mut c_char;
*/
pub fn dt_from_source(path: &Path) -> Result<&mut BootInfo, String> {
	unsafe {
		the_boot_info = None;
		treesource_error = false;
	}

	if unsafe{ yyparse() != 0 } {
		Err(String::from("Unable to parse input tree"))
	} else if unsafe { treesource_error } {
		Err(String::from("Syntax error parsing input tree"))
	} else if let Some(boot_info) = unsafe{ ptr::read(&the_boot_info) } {
		boot_info.fill_fullpaths();
		boot_info.check();
		Ok(boot_info)
	} else {
		Err(String::from("Boot info is null"))
	}
}

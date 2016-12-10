#ifndef _DTC_H
#define _DTC_H

/*
 * (C) Copyright David Gibson <dwg@au1.ibm.com>, IBM Corporation.  2005.
 * Modified by Gabriel Smith <ga29smith@gmail.com>
 *
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
 *                                                                   USA
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdarg.h>
#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <unistd.h>

#ifdef DEBUG
#define debug(...)	printf(__VA_ARGS__)
#else
#define debug(...)
#endif

#define DEFAULT_FDT_VERSION	17

/* DT constraints */
#define MAX_PROPNAME_LEN	31
#define MAX_NODENAME_LEN	31

/*
 * Command line options
 */
extern int phandle_format;	/* Use linux,phandle or phandle properties */

#define PHANDLE_LEGACY	0x1
#define PHANDLE_EPAPR	0x2
#define PHANDLE_BOTH	0x3

typedef uint32_t cell_t;

struct boot_info;
struct vec_re_info;
struct reserve_info;
struct fdt_reserve_entry;
struct vec_node;
struct node;
struct vec_prop;
struct property;
struct data {
	int len;
	char *val;
	struct marker *markers; //actually a Vec<Marker>, but don't tell C
};
struct label;
struct marker;

enum markertype {
	REF_PHANDLE,
	REF_PATH,
	LABEL,
};

void data_free(struct data *d);
struct data *data_copy_escape_string(const char *s, int len);
struct data *data_copy_file(FILE *f, size_t len);
struct data *data_merge(struct data *d1, struct data *d2);
struct data *data_append_integer(struct data *d, uint64_t word, int bits);
struct data *data_append_byte(struct data *d, uint8_t byte);
struct data *data_add_marker(struct data *d, enum markertype type, char *ref);

void memreserve_add_label(struct reserve_info *info, char *label);
void node_add_label(struct node *node, char *label);
void vec_node_add_label(struct vec_node *node, char *label);
void property_add_label(struct property *prop, char *label);

struct property *build_property(char *name, struct data *val);
struct property *build_property_delete(char *name);
struct vec_prop *chain_property(struct property *first, struct vec_prop *list);

struct node *build_node(struct vec_prop *proplist, struct vec_node *children);
struct node *build_node_delete(void);
struct node *name_node(struct node *node, char *name);
struct vec_node *chain_node(struct node *first, struct vec_node *list);
struct node *merge_nodes(struct node *old_node, struct node *new_node);
void delete_node(struct node *node);
struct node *get_node_by_ref(struct node *tree, const char *ref);

uint32_t guess_boot_cpuid(struct node *tree);

struct reserve_info *build_reserve_entry(uint64_t start, uint64_t len);
struct vec_re_info *chain_reserve_entry(struct reserve_info *first,
							struct vec_re_info *list);

struct boot_info *build_boot_info(struct vec_re_info *reserve_list,
						struct node *tree, uint32_t boot_cpuid_phys);

char *xstrdup(const char *s);

static inline void __attribute__((noreturn)) die(const char *str, ...) {
	va_list ap;

	va_start(ap, str);
	fprintf(stderr, "FATAL ERROR: ");
	vfprintf(stderr, str, ap);
	va_end(ap);
	exit(1);
}

static inline void *xmalloc(size_t len) {
	void *new = malloc(len);

	if (!new)
		die("malloc() failed\n");

	return new;
}

#endif /* _DTC_H */

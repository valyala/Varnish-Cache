/*
 * Written by Poul-Henning Kamp <phk@phk.freebsd.dk>
 *
 * This file is in the public domain.
 *
 */

#ifndef MINIOBJ_H_INCLUDED
#define MINIOBJ_H_INCLUDED

#include <stdint.h>	/* for SIZE_MAX */
#include <stdlib.h>	/* for malloc(), etc. */
#include <string.h>	/* for strdup() */

#include "vas.h"

#ifndef WITHOUT_MAGIC

#define MAGIC_INITIALIZER(type_magic)	.magic = (type_magic),

#define CMP_MAGIC(ptr, type_magic)					\
	((ptr)->magic == (type_magic))

#define SET_MAGIC(ptr, type_magic)					\
	do {								\
		(ptr)->magic = (type_magic);				\
	} while (0)

#else

#define MAGIC_INITIALIZER(type_magic)	/* nothing */
#define CMP_MAGIC(ptr, type_magic)	1	/* always true */
#define SET_MAGIC(ptr, type_magic)	do { /* nothing */ } while (0)

#endif	/* USE_MAGIC */

#define MAGIC_HERE			unsigned magic

#define MALLOC_NOTNULL(to, size)					\
	do {								\
		assert((size) > 0);					\
		(to) = malloc((size));					\
		XXXAN((to));						\
	} while (0)

#define CALLOC_NOTNULL(to, items_count)					\
	do {								\
		assert((items_count) > 0);				\
		(to) = calloc((items_count), sizeof(*(to)));		\
		XXXAN((to));						\
	} while (0)

static inline void *
realloc_array_notnull(void *a, size_t items_count, size_t item_size)
{
	/* a can be NULL */
	assert(items_count > 0);
	assert(item_size > 0);
	xxxassert(items_count <= SIZE_MAX / item_size);
	a = realloc(a, items_count * item_size);
	XXXAN(a);
	return a;
}

#define REALLOC_ARRAY_NOTNULL(to, items_count)				\
	do {								\
		(to) = realloc_array_notnull((to), (items_count),	\
					      sizeof(*(to)));		\
	} while (0)

#define FREE_ORNULL(ptr)						\
	do {								\
		free((ptr));						\
	} while (0)

#define FREE_NOTNULL(ptr)						\
	do {								\
		AN((ptr));						\
		FREE_ORNULL((ptr));					\
	} while (0)

#define ALLOC_OBJ_NOTNULL(to, type_magic)				\
	do {								\
		CALLOC_NOTNULL((to), 1);				\
		SET_MAGIC((to), (type_magic));				\
	} while (0)

#define FREE_OBJ_NOTNULL(ptr, type_magic)				\
	do {								\
		CHECK_OBJ_NOTNULL((ptr), (type_magic));			\
		SET_MAGIC((ptr), 0);					\
		FREE_NOTNULL((ptr));					\
	} while (0)

#define VALID_OBJ(ptr, type_magic)					\
	((ptr) != NULL && CMP_MAGIC((ptr), (type_magic)))

#define CHECK_OBJ_NOTNULL(ptr, type_magic)				\
	do {								\
		AN((ptr));						\
		AN(CMP_MAGIC((ptr), (type_magic)));			\
	} while (0)

#define CHECK_OBJ_ORNULL(ptr, type_magic)				\
	do {								\
		if ((ptr) != NULL)					\
			CHECK_OBJ_NOTNULL((ptr), (type_magic));		\
	} while (0)

#define CAST_OBJ_NOTNULL(to, from, type_magic)				\
	do {								\
		AN((from));						\
		(to) = (from);						\
		CHECK_OBJ_NOTNULL((to), (type_magic));			\
	} while (0)

static inline char *
strdup_notnull(const char *s)
{
	char *p;
	AN(s);
	p = strdup(s);
	XXXAN(p);
	return p;
}

#define REPLACE(to, from)						\
	do {								\
		if ((to) != NULL)					\
			FREE_NOTNULL((to));				\
		if ((from) != NULL)					\
			(to) = strdup_notnull((from));			\
		else							\
			(to) = NULL;					\
	} while (0)

#define ZERO_OBJ(ptr)							\
	do {								\
		memset(ptr, 0, sizeof(*(ptr)));				\
	} while (0)

static inline void
zero_array(void *a, size_t items_count, size_t item_size)
{
	assert(items_count > 0);
	assert(item_size > 0);
	memset(a, 0, items_count * item_size);
}

#define ZERO_ARRAY(a, items_count)					\
	do {								\
		zero_array((a), (items_count), sizeof(*(a)));		\
	} while (0)

#define INIT_OBJ(ptr, type_magic)					\
	do {								\
		ZERO_OBJ((ptr));					\
		SET_MAGIC((ptr), (type_magic));				\
	} while (0)

#endif

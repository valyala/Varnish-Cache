/*
 * Written by Poul-Henning Kamp <phk@phk.freebsd.dk>
 *
 * This file is in the public domain.
 *
 */

#define __CMP_MAGIC(ptr, type_magic)					\
	((ptr)->magic == (type_magic))

#define __SET_MAGIC(ptr, type_magic)					\
	do {								\
		(ptr)->magic = (type_magic);				\
	} while (0)

#define __CHECK_MAGIC(ptr, type_magic)					\
	do {								\
		assert(__CMP_MAGIC((ptr), (type_magic)));		\
	} while (0)

#define ALLOC_OBJ_NOTNULL(to, type_magic)				\
	do {								\
		(to) = calloc(1, sizeof(*(to)));			\
		XXXAN((to));						\
		__SET_MAGIC((to), (type_magic));			\
	} while (0)

#define FREE_OBJ(to)							\
	do {								\
		__SET_MAGIC((to), 0);					\
		free(to);						\
	} while (0)

#define VALID_OBJ(ptr, type_magic)					\
	((ptr) != NULL && __CMP_MAGIC((ptr), (type_magic)))

#define CHECK_OBJ_NOTNULL(ptr, type_magic)				\
	do {								\
		assert((ptr) != NULL);					\
		__CHECK_MAGIC((ptr), (type_magic));			\
	} while (0)

#define CHECK_OBJ_ORNULL(ptr, type_magic)				\
	do {								\
		if ((ptr) != NULL)					\
			__CHECK_MAGIC((ptr), (type_magic));		\
	} while (0)

#define CAST_OBJ_NOTNULL(to, from, type_magic)				\
	do {								\
		(to) = (from);						\
		assert((to) != NULL);					\
		__CHECK_MAGIC((to), (type_magic));			\
	} while (0)

#define REPLACE(ptr, val)						\
	do {								\
		if ((ptr) != NULL)					\
			free(ptr);					\
		if ((val) != NULL) {					\
			ptr = strdup(val);				\
			XXXAN((ptr));					\
		} else {						\
			ptr = NULL;					\
		}							\
	} while (0)

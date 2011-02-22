#ifndef _CLISP_H
#define _CLISP_H

/* unixconf */
#define ABORT_VOLATILE 
#define ASM_UNDERSCORE 
#define CADDR_T caddr_t
#define CODE_ADDRESS_RANGE 0x00000000UL
#define CONNECT_ADDRLEN_T int
#define CONNECT_CONST const
#define CONNECT_NAME_T struct sockaddr *
#define DIRENT 
#define ELOOP_VALUE ELOOP
#define GETCWD_SIZE_T size_t
#define GETTIMEOFDAY_TZP_T struct timezone *
#define HAVE_ALLOCA_H 
#define HAVE_ARPA_INET_H 1
#define HAVE_DECL_ENVIRON 0
#define HAVE_DLCLOSE 1
#define HAVE_DLERROR 1
#define HAVE_DLFCN_H 1
#define HAVE_DLOPEN 1
#define HAVE_DLSYM 1
#define HAVE_FIONREAD 
#define HAVE_FLOCK 1
#define HAVE_FORK 1
#define HAVE_FSYNC 1
#define HAVE_GETCWD 
#define HAVE_GETHOSTBYNAME 
#define HAVE_GETHOSTENT 1
#define HAVE_GETHOSTNAME 1
#define HAVE_GETPAGESIZE 
#define HAVE_GETRLIMIT 1
#define HAVE_GETRUSAGE 
#define HAVE_GETTIMEOFDAY 1
#define HAVE_ICONV 1
#define HAVE_INET_NTOP 1
#define HAVE_INET_PTON 1
#define HAVE_INTTYPES_H 1
#define HAVE_IPV4 
#define HAVE_IPV6 
#define HAVE_LBER_H 1
#define HAVE_LC_MESSAGES 1
#define HAVE_LDAP_H 1
#define HAVE_LONGLONG 
#define HAVE_LSTAT 
#define HAVE_MEMORY_H 1
#define HAVE_MEMSET 1
#define HAVE_MMAP 
#define HAVE_MMAP_ANON 
#define HAVE_MPROTECT 1
#define HAVE_MSYNC 1
#define HAVE_MUNMAP 1
#define HAVE_NETDB_H 1
#define HAVE_NETINET_IN_H 1
#define HAVE_NICE 1
#define HAVE_OFFSETOF 
#define HAVE_PERROR_DECL 
#define HAVE_PUTENV 1
#define HAVE_RAISE 
#define HAVE_READLINK 1
#define HAVE_RELIABLE_FIONREAD 
#define HAVE_SELECT 1
#define HAVE_SETENV 1
#define HAVE_SETITIMER 1
#define HAVE_SETPGID 1
#define HAVE_SETRLIMIT 1
#define HAVE_SETSID 1
#define HAVE_SGTTY_H 1
#define HAVE_SHUTDOWN 1
#define HAVE_SIGACTION 1
#define HAVE_SIGINTERRUPT 1
#define HAVE_SOCKADDR_UN_LEN 
#define HAVE_STDBOOL_H 1
#define HAVE_STDINT_H 1
#define HAVE_STDLIB_H 1
#define HAVE_STRERROR 1
#define HAVE_STRINGS_H 1
#define HAVE_STRING_H 1
#define HAVE_STRUCT_DIRENT_D_NAMLEN 1
#define HAVE_SYS_FILE_H 1
#define HAVE_SYS_IPC_H 1
#define HAVE_SYS_RESOURCE_H 1
#define HAVE_SYS_SELECT_H 
#define HAVE_SYS_SHM_H 1
#define HAVE_SYS_STAT_H 1
#define HAVE_SYS_TIMES_H 1
#define HAVE_SYS_TYPES_H 1
#define HAVE_SYS_UN_H 1
#define HAVE_SYS_UTSNAME_H 
#define HAVE_TCGETATTR 
#define HAVE_TCSAFLUSH 
#define HAVE_TERMIOS_H 1
#define HAVE_UALARM 1
#define HAVE_UNISTD_H 1
#define HAVE_USLEEP 1
#define HAVE_VFORK 1
#define HAVE_WORKING_FORK 1
#define HAVE_WORKING_MPROTECT 
#define HAVE_WORKING_VFORK 1
#define HAVE__JMP 
#define ICONV_CONST const
#define INET_ADDR_CONST const
#define INET_ADDR_SUFFIX 
#define IOCTL_DOTS 
#define IOCTL_REQUEST_T unsigned long
#define MALLOC_ADDRESS_RANGE 0x01000000UL
#define NEED_SYS_FILIO_H 
#define PACKAGE_BUGREPORT "http://clisp.cons.org/"
#define PACKAGE_NAME "GNU CLISP"
#define PACKAGE_STRING "GNU CLISP 2.33.2 (2004-06-02)"
#define PACKAGE_TARNAME "clisp"
#define PACKAGE_VERSION "2.33.2 (2004-06-02)"
#define PID_T pid_t
#define RETABORTTYPE void
#define RETCLOSEDIRTYPE int
#define RETGETPAGESIZETYPE int
#define RETSIGTYPE void
#define RET_INET_ADDR_TYPE unsigned int
#define RLIMIT_RESOURCE_T int
#define RUSAGE_WHO_T int
#define SELECT_CONST 
#define SELECT_SET_T fd_set
#define SELECT_WIDTH_T int
#define SETRLIMIT_CONST const
#define SETSOCKOPT_ARG_T void*
#define SETSOCKOPT_CONST const
#define SETSOCKOPT_OPTLEN_T int
#define SHLIB_ADDRESS_RANGE 0xA0000000UL
#define SIGNALBLOCK_BSD 
#define SIGNALBLOCK_POSIX 
#define SIGNALBLOCK_SYSV 
#define SIGNAL_NEED_UNBLOCK 
#define SIZEOF_INO_T 4
#define SIZEOF_OFF_T 8
#define SOCKLEN_T socklen_t
#define STACK_ADDRESS_RANGE 0xBF000000UL
#define STDC_HEADERS 1
#define VALID_FILENAME_CHAR ((ch >= 1) && (ch <= 127) && (ch != 47))
#ifndef _ALL_SOURCE
#endif
#ifndef __CHAR_UNSIGNED__
#endif
#ifndef __cplusplus
#endif
#define return_void return

/* intparam.h */
#define char_bitsize 8
#define short_bitsize 16
#define int_bitsize 32
#define long_bitsize 32
#define long_long_bitsize 64
#define pointer_bitsize 32
#define sizeof_char 1
#define alignment_char 1
#define sizeof_short 2
#define alignment_short 2
#define sizeof_int 4
#define alignment_int 4
#define sizeof_long 4
#define alignment_long 4
#define sizeof_long_long 8
#define alignment_long_long 4
#define sizeof_float 4
#define alignment_float 4
#define sizeof_double 8
#define alignment_double 4
#define short_big_endian
#define int_big_endian
#define long_big_endian
#define long_long_big_endian
#define stack_grows_down

/* floatparam.h */
#define rounds_to_nearest        0  /* 0.5 ulp */
#define rounds_to_zero           1  /* 1 ulp */
#define rounds_to_infinity       2  /* 1 ulp */
#define rounds_to_minus_infinity 3  /* 1 ulp */
#define float_mant_bits 24
#define float_rounds rounds_to_nearest
#define float_rounds_correctly 1
#define double_mant_bits 53
#define double_rounds rounds_to_nearest
#define double_rounds_correctly 1

/* genclisph */
#define SAFETY 0
#define CLISP_UNICODE 1
#define UNIX
#define BIG_ENDIAN_P  1UL
#ifdef __cplusplus
#define BEGIN_DECLS  extern "C" {
#define END_DECLS    }
#else
#define BEGIN_DECLS
#define END_DECLS
#endif
#define CONCAT_(xxx,yyy)  xxx##yyy
#define CONCAT3_(aaa,bbb,ccc)  aaa##bbb##ccc
#define CONCAT(xxx,yyy)  CONCAT_(xxx,yyy)
#define CONCAT3(aaa,bbb,ccc)  CONCAT3_(aaa,bbb,ccc)
#define STRING(token) #token
#define STRINGIFY(token) STRING(token)
#define nonreturning_function(storclass,funname,arguments)  \
  storclass void funname arguments
#define var
#define NOTREACHED  fehler_notreached(__FILE__,__LINE__)
#define ASSERT(expr)  do { if (!(expr)) NOTREACHED; } while(0)
#define alloca  __builtin_alloca
typedef char SBYTE;
typedef unsigned char UBYTE;
typedef short SWORD;
typedef unsigned short UWORD;
typedef long SLONG;
typedef unsigned long ULONG;
typedef long long SLONGLONG;
typedef unsigned long long ULONGLONG;
#include <stdbool.h>
#undef NULL
#define NULL  ((void*) 0L)
#define unspecified 0
#define pointerplus(pointer,offset)  ((UBYTE*)(pointer)+(offset))
#define bit(n)  (1L<<(n))
#define bit_test(x,n)  ((x) & bit(n))
#define minus_bit(n)  (-1L<<(n))
#define DYNAMIC_ARRAY(arrayvar,arrayeltype,arraysize)  arrayeltype arrayvar[arraysize]
#define FREE_DYNAMIC_ARRAY(arrayvar)
typedef UBYTE uint1;
typedef SBYTE sint1;
typedef UBYTE uint2;
typedef SBYTE sint2;
typedef UBYTE uint3;
typedef SBYTE sint3;
typedef UBYTE uint4;
typedef SBYTE sint4;
typedef UBYTE uint5;
typedef SBYTE sint5;
typedef UBYTE uint6;
typedef SBYTE sint6;
typedef UBYTE uint7;
typedef SBYTE sint7;
typedef UBYTE uint8;
typedef SBYTE sint8;
typedef UWORD uint9;
typedef SWORD sint9;
typedef UWORD uint10;
typedef SWORD sint10;
typedef UWORD uint11;
typedef SWORD sint11;
typedef UWORD uint12;
typedef SWORD sint12;
typedef UWORD uint13;
typedef SWORD sint13;
typedef UWORD uint14;
typedef SWORD sint14;
typedef UWORD uint15;
typedef SWORD sint15;
typedef UWORD uint16;
typedef SWORD sint16;
typedef ULONG uint17;
typedef SLONG sint17;
typedef ULONG uint18;
typedef SLONG sint18;
typedef ULONG uint19;
typedef SLONG sint19;
typedef ULONG uint20;
typedef SLONG sint20;
typedef ULONG uint21;
typedef SLONG sint21;
typedef ULONG uint22;
typedef SLONG sint22;
typedef ULONG uint23;
typedef SLONG sint23;
typedef ULONG uint24;
typedef SLONG sint24;
typedef ULONG uint25;
typedef SLONG sint25;
typedef ULONG uint26;
typedef SLONG sint26;
typedef ULONG uint27;
typedef SLONG sint27;
typedef ULONG uint28;
typedef SLONG sint28;
typedef ULONG uint29;
typedef SLONG sint29;
typedef ULONG uint30;
typedef SLONG sint30;
typedef ULONG uint31;
typedef SLONG sint31;
typedef ULONG uint32;
typedef SLONG sint32;
typedef ULONGLONG uint33;
typedef SLONGLONG sint33;
typedef ULONGLONG uint48;
typedef SLONGLONG sint48;
typedef ULONGLONG uint64;
typedef SLONGLONG sint64;
typedef sint8 sintB;
typedef uint8 uintB;
typedef uint16 uintW;
typedef sint32 sintL;
typedef uint32 uintL;
typedef sint32 sintP;
typedef uint32 uintP;
typedef uint32 uintWL;
typedef uint32 uintBWL;
#define uintC uintWL
typedef uint32 uintD;
#include <stdlib.h>
#define HEAPCODES
typedef void * gcv_object_t;
typedef uintP oint;
typedef sintP soint;
#define as_oint(expr)  (oint)(expr)
#define as_object(o)  (gcv_object_t)(o)
typedef uint8 tint;
typedef uint32 aint;
typedef gcv_object_t object;
#define objectplus(obj,offset)  ((object)pointerplus(obj,offset))
#define wbit_test  bit_test
#define minus_wbit  minus_bit
#define type_data_object(type,data)  (as_object(((oint)(tint)(type) << 0) + ((oint)(aint)(data) << 7)))
#define type_zero_oint(type)  ((oint)(tint)(type) << 0)
#define immediate_object_p(obj)  ((7 & ~as_oint(obj)) == 0)
#define gcinvariant_object_p(obj)  (((as_oint(obj) & 1) == 0) || immediate_object_p(obj))
#define gcinvariant_bias_p(bias)  ((((bias) & 1) == 0) || ((7 & ~(bias)) == 0))
#define varobjects_misaligned  0UL
#define VAROBJECTS_ALIGNMENT_DUMMY_DECL
#define varobject_alignment  4UL
#define VAROBJECT_HEADER  object GCself; uintL tfl;
#define varobject_type(ptr) ((sintB)((ptr)->tfl & 0xFF))
typedef struct { VAROBJECT_HEADER gcv_object_t recdata[unspecified]; } record_;
typedef record_ * Record;
#define record_type(ptr)  varobject_type(ptr)
#define Record_type(obj)  record_type(TheRecord(obj))
#define record_flags(ptr)  (((ptr)->tfl >> 8) & 0xFF)
#define Record_flags(obj)  record_flags(TheRecord(obj))
#define LRECORD_HEADER  VAROBJECT_HEADER
typedef struct { LRECORD_HEADER } lrecord_;
typedef lrecord_ * Lrecord;
#define SRECORD_HEADER  VAROBJECT_HEADER
typedef struct { SRECORD_HEADER object recdata[unspecified]; } srecord_;
typedef srecord_ * Srecord;
#define srecord_length(ptr)  ((ptr)->tfl >> 16)
#define XRECORD_HEADER  VAROBJECT_HEADER
typedef struct { gcv_object_t cdr; gcv_object_t car; } cons_;
typedef cons_ * Cons;
typedef struct { VAROBJECT_HEADER gcv_object_t symvalue; gcv_object_t symfunction; gcv_object_t proplist; gcv_object_t pname; gcv_object_t homepackage; } symbol_;
typedef symbol_ * Symbol;
typedef uint24 cint;
#define int_char(int_from_int_char)  type_data_object(39,(aint)(cint)(int_from_int_char))
#define char_int(char_from_char_int)  ((cint)(as_oint(char_from_char_int)>>7UL))
typedef cint chart;
#define as_cint(ch)  (ch)
#define as_chart(c)  (c)
#define code_char(ch)  int_char(as_cint(ch))
#define char_code(obj)  as_chart(char_int(obj))
#define fixnum(x)  type_data_object(7,x)
#define Fixnum_0  fixnum(0)
#define Fixnum_1  fixnum(1)
#define Fixnum_minus1  type_data_object(15,0xFFFFFFUL)
#define posfixnum_to_L(obj)  ((uintL)((as_oint(obj)&0x7FFFFFFFUL)>>7UL))
#define fixnum_to_L(obj)  (sintL)( ((((sintL)as_oint(obj) >> 3UL) << 31UL) >> 7UL) | ((uintL)((as_oint(obj) & 0x7FFFFFFFUL) >> 7UL)) )
#define fixnum_inc(obj,delta)  objectplus(obj, (soint)(delta) << 7)
#define posfixnum(x)  fixnum_inc(Fixnum_0,x)
#define negfixnum(x)  fixnum_inc(fixnum_inc(Fixnum_minus1,1),x)
#define sfixnum(x) ((x)>=0 ? posfixnum(x) : negfixnum(x))
typedef struct { VAROBJECT_HEADER uintD data[unspecified]; } bignum_;
typedef bignum_ * Bignum;
#define bignum_length(ptr)  srecord_length(ptr)
#define Bignum_length(obj)  bignum_length(TheBignum(obj))
typedef uint32 ffloat;
typedef union { ffloat eksplicit; } ffloatjanus;
typedef struct {uint32 semhi,mlo;} dfloat;
typedef union { dfloat eksplicit; } dfloatjanus;
typedef struct { LRECORD_HEADER uint8  data[unspecified]; } sbvector_;
typedef sbvector_ * Sbvector;
typedef struct { LRECORD_HEADER uint32  data[unspecified]; } sstring_;
typedef sstring_ * Sstring;
typedef struct { LRECORD_HEADER gcv_object_t data[unspecified]; } svector_;
typedef svector_ * Svector;
#define lrecord_length(ptr)  ((ptr)->tfl >> 8)
#define sarray_length(ptr)  lrecord_length(ptr)
#define Sarray_length(obj)  sarray_length(TheSarray(obj))
#define sbvector_length(ptr)  sarray_length(ptr)
#define Sbvector_length(obj)  sbvector_length(TheSbvector(obj))
#define sstring_length(ptr)  sarray_length(ptr)
#define Sstring_length(obj)  sstring_length(TheSstring(obj))
extern bool string_equal (object string1, object string2);
typedef Srecord Structure;
#define structure_types   recdata[0]
typedef struct { SRECORD_HEADER gcv_object_t inst_class; gcv_object_t inst_cl_id; gcv_object_t other[unspecified]; } * Instance;
typedef void Values;
typedef Values (*lisp_function_t)();
typedef struct { XRECORD_HEADER gcv_object_t name; gcv_object_t keywords; lisp_function_t function; uintW argtype; uintW req_anz; uintW opt_anz; uintB rest_flag; uintB key_flag; uintW key_anz; uintW seclass; } subr_t;
typedef subr_t * Subr;
typedef enum { subr_norest, subr_rest } subr_rest_t;
typedef enum { subr_nokey, subr_key, subr_key_allow } subr_key_t;
#define make_machine(ptr)  as_object((oint)(ptr)+0UL)
#define make_system(data)  type_data_object(63,0x800001UL | (0xFFFFFFUL & (data)))
#define unbound  make_system(0xffffff)
#define nullobj  make_machine(0)
#define cgci_pointable(obj)  as_oint(obj)
#define pgci_pointable(obj)  as_oint(obj)
#define ngci_pointable(obj)  as_oint(obj)
#define TheCons(obj)  ((Cons)(ngci_pointable(obj)-3UL))
#define TheSymbol(obj)  ((Symbol)(ngci_pointable(obj)-1UL))
#define TheBignum(obj)  ((Bignum)(ngci_pointable(obj)-1UL))
#define TheSbvector(obj)  ((Sbvector)(ngci_pointable(obj)-1UL))
#define TheSstring(obj)  ((Sstring)(ngci_pointable(obj)-1UL))
#define TheSvector(obj)  ((Svector)(ngci_pointable(obj)-1UL))
#define TheRecord(obj)  ((Record)(ngci_pointable(obj)-1UL))
#define TheSrecord(obj)  ((Srecord)(ngci_pointable(obj)-1UL))
#define TheStructure(obj)  ((Structure)(ngci_pointable(obj)-1UL))
#define TheInstance(obj)  ((Instance)(ngci_pointable(obj)-1UL))
#define TheSubr(obj)  ((Subr)(cgci_pointable(obj)-2UL))
#define Car(obj)  (TheCons(obj)->car)
#define Cdr(obj)  (TheCons(obj)->cdr)
#define Symbol_value(obj)  (TheSymbol(obj)->symvalue)
#define Symbol_function(obj)  (TheSymbol(obj)->symfunction)
#define Symbol_plist(obj)  (TheSymbol(obj)->proplist)
#define Symbol_name(obj)  (TheSymbol(obj)->pname)
#define Symbol_package(obj)  (TheSymbol(obj)->homepackage)
#define eq(obj1,obj2)  ((obj1) == (obj2))
#define nullp(obj)  (eq(obj,NIL))
#define boundp(obj) (!eq(obj,unbound))
#define missingp(obj) (!boundp(obj) || nullp(obj))
#define consp(obj)  ((as_oint(obj) & 7UL) == 3UL)
#define mconsp(obj)  consp(obj)
#define atomp(obj)  (!consp(obj))
#define matomp(obj)  atomp(obj)
#define listp(obj)  (nullp(obj) || consp(obj))
#define varobjectp(obj)  ((as_oint(obj) & 3UL) == 1UL)
#define symbolp(obj)  (varobjectp(obj) && (Record_type(obj) == 33UL))
#define builtin_stream_p(obj) (orecordp(obj) && (Record_type(obj) == 39))
#define vectorp(obj)  (varobjectp(obj) && ((uintB)(Record_type(obj) - 1UL) <= 23UL))
#define simple_vector_p(obj)  (varobjectp(obj) && (Record_type(obj) == 9UL))
#define general_vector_p(obj)  (varobjectp(obj) && ((Record_type(obj) == 9UL) || (Record_type(obj) == 1UL && (Iarray_flags(obj) & 15UL) == 6UL)))
#define simple_string_p(obj)  (varobjectp(obj) && ((uintB)(Record_type(obj) - 17) <= 6))
#define stringp(obj)  (varobjectp(obj) && ((uintB)(Record_type(obj) - 17) <= 6))
#define simple_bit_vector_p(atype,obj)  (varobjectp(obj) && (Record_type(obj) == 10UL+(atype)))
#define bit_vector_p(atype,obj)  (varobjectp(obj) && ((Record_type(obj) & ~8UL) == 2UL+(atype)))
#define arrayp(obj)  (varobjectp(obj) && ((uintB)(Record_type(obj)-1UL) <= 24UL))
extern object array_displace_check (object array, uintL size, uintL* index);
extern uintL vector_length (object vector);
#define instancep(obj)  (varobjectp(obj) && (Record_type(obj) == 4294967295UL))
#define orecordp(obj)  varobjectp(obj)
#define structurep(obj)  (orecordp(obj) && (Record_type(obj) == -2))
#define charp(obj)  ((as_oint(obj) & 63) == 39)
#define integerp(obj)  (((as_oint(obj) & 55UL) == 7UL) || (varobjectp(obj) && (Record_type(obj) == 26UL)))
#define fixnump(obj)  ((as_oint(obj) & 55UL) == 7UL)
#define posfixnump(obj)  ((as_oint(obj) & 63UL) == 7UL)
#define bignump(obj)  (varobjectp(obj) && (Record_type(obj) == 26UL))
#define posbignump(obj)  (varobjectp(obj) && (Record_type(obj) == 26UL) && ((Record_flags(obj) & bit(7)) == 0))
#define positivep(obj)  ((as_oint(obj) & wbit(1)) ? (as_oint(obj) & 8UL) == 0 : (Record_flags(obj) & 128UL) == 0)
#define FN_positivep(obj)  ((as_oint(obj) & 8UL) == 0)
#define BN_positivep(obj)  ((Record_flags(obj) & 128UL) == 0)
#define uint8_p(obj)  ((as_oint(obj) & ~0x7F80UL) == 0x7UL)
#define sint8_p(obj)  (((as_oint(obj) ^ (FN_positivep(obj) ? 0 : 0x7FFFFF88UL)) & ~0x3F80UL) == 0x7UL)
#define uint16_p(obj)  ((as_oint(obj) & ~0x7FFF80UL) == 0x7UL)
#define sint16_p(obj)  (((as_oint(obj) ^ (FN_positivep(obj) ? 0 : 0x7FFFFF88UL)) & ~0x3FFF80UL) == 0x7UL)
#define uint32_p(obj)  (posfixnump(obj) || (posbignump(obj) && (Bignum_length(obj) <= 2UL) && ((Bignum_length(obj) < 2UL) || (TheBignum(obj)->data[0] < (uintD)bit(0UL)) )))
#define sint32_p(obj)  (fixnump(obj) || (bignump(obj) && (Bignum_length(obj) <= 1UL) && ((Bignum_length(obj) < 1UL) || ((TheBignum(obj)->data[0] ^ (BN_positivep(obj) ? (uintD)0 : ~(uintD)0)) < (uintD)bit(31UL)) )))
#define uint64_p(obj)  (posfixnump(obj) || (posbignump(obj) && (Bignum_length(obj) <= 3UL) && ((Bignum_length(obj) < 3UL) || (TheBignum(obj)->data[0] < (uintD)bit(0UL)) )))
#define sint64_p(obj)  (fixnump(obj) || (bignump(obj) && (Bignum_length(obj) <= 2UL) && ((Bignum_length(obj) < 2UL) || ((TheBignum(obj)->data[0] ^ (BN_positivep(obj) ? (uintD)0 : ~(uintD)0)) < (uintD)bit(31UL)) )))
extern gcv_object_t* STACK;
#define begin_call()
#define end_call()
#define begin_callback()  end_call()
#define end_callback() 
#define begin_system_call()  begin_call()
#define end_system_call()  end_call()
nonreturning_function(extern, fehler_notreached, (const char * file, uintL line));
#define GETTEXT(english) english
extern object allocate_cons (void);
extern object allocate_vector (uintL len);
#define Atype_32Bit 5
#define Atype_8Bit 3
#define Atype_Bit 0
extern object allocate_bit_vector (uintB atype, uintL len);
extern object allocate_s32string (uintL len);
#define allocate_string(len)  allocate_s32string(len)
#define asciz_length(a)  ((uintL)__builtin_strlen(a))
extern struct subr_tab_ {
  VAROBJECTS_ALIGNMENT_DUMMY_DECL
  subr_t D_funtabref;
  subr_t D_subr_info;
  subr_t D_special_variable_p;
  subr_t D_add_implicit_block;
  subr_t D_function_block_name;
  subr_t D_copy_simple_vector;
  subr_t D_vector;
  subr_t D_aref;
  subr_t D_store;
  subr_t D_svref;
  subr_t D_svstore;
  subr_t D_psvstore;
  subr_t D_row_major_aref;
  subr_t D_row_major_store;
  subr_t D_array_element_type;
  subr_t D_array_rank;
  subr_t D_array_dimension;
  subr_t D_array_dimensions;
  subr_t D_array_total_size;
  subr_t D_array_in_bounds_p;
  subr_t D_array_row_major_index;
  subr_t D_adjustable_array_p;
  subr_t D_array_displacement;
  subr_t D_bit;
  subr_t D_sbit;
  subr_t D_bit_and;
  subr_t D_bit_ior;
  subr_t D_bit_xor;
  subr_t D_bit_eqv;
  subr_t D_bit_nand;
  subr_t D_bit_nor;
  subr_t D_bit_andc1;
  subr_t D_bit_andc2;
  subr_t D_bit_orc1;
  subr_t D_bit_orc2;
  subr_t D_bit_not;
  subr_t D_array_has_fill_pointer_p;
  subr_t D_fill_pointer;
  subr_t D_set_fill_pointer;
  subr_t D_vector_push;
  subr_t D_vector_pop;
  subr_t D_vector_push_extend;
  subr_t D_make_array;
  subr_t D_adjust_array;
  subr_t D_vector_init;
  subr_t D_vector_upd;
  subr_t D_vector_endtest;
  subr_t D_vector_fe_init;
  subr_t D_vector_fe_upd;
  subr_t D_vector_fe_endtest;
  subr_t D_vector_length;
  subr_t D_vector_init_start;
  subr_t D_vector_fe_init_end;
  subr_t D_make_bit_vector;
  subr_t D_string_info;
  subr_t D_standard_char_p;
  subr_t D_graphic_char_p;
  subr_t D_char_width;
  subr_t D_string_char_p;
  subr_t D_alpha_char_p;
  subr_t D_upper_case_p;
  subr_t D_lower_case_p;
  subr_t D_both_case_p;
  subr_t D_digit_char_p;
  subr_t D_alphanumericp;
  subr_t D_char_gleich;
  subr_t D_char_ungleich;
  subr_t D_char_kleiner;
  subr_t D_char_groesser;
  subr_t D_char_klgleich;
  subr_t D_char_grgleich;
  subr_t D_char_equal;
  subr_t D_char_not_equal;
  subr_t D_char_lessp;
  subr_t D_char_greaterp;
  subr_t D_char_not_greaterp;
  subr_t D_char_not_lessp;
  subr_t D_char_code;
  subr_t D_code_char;
  subr_t D_character;
  subr_t D_char_upcase;
  subr_t D_char_downcase;
  subr_t D_digit_char;
  subr_t D_char_int;
  subr_t D_int_char;
  subr_t D_char_name;
  subr_t D_char;
  subr_t D_schar;
  subr_t D_store_char;
  subr_t D_store_schar;
  subr_t D_string_gleich;
  subr_t D_string_ungleich;
  subr_t D_string_kleiner;
  subr_t D_string_groesser;
  subr_t D_string_klgleich;
  subr_t D_string_grgleich;
  subr_t D_string_equal;
  subr_t D_string_not_equal;
  subr_t D_string_lessp;
  subr_t D_string_greaterp;
  subr_t D_string_not_greaterp;
  subr_t D_string_not_lessp;
  subr_t D_search_string_gleich;
  subr_t D_search_string_equal;
  subr_t D_make_string;
  subr_t D_string_both_trim;
  subr_t D_string_width;
  subr_t D_nstring_upcase;
  subr_t D_string_upcase;
  subr_t D_nstring_downcase;
  subr_t D_string_downcase;
  subr_t D_nstring_capitalize;
  subr_t D_string_capitalize;
  subr_t D_string;
  subr_t D_name_char;
  subr_t D_substring;
  subr_t D_string_concat;
  subr_t D_exit;
  subr_t D_psymbol_value;
  subr_t D_symbol_value;
  subr_t D_symbol_function;
  subr_t D_fdefinition;
  subr_t D_boundp;
  subr_t D_fboundp;
  subr_t D_special_operator_p;
  subr_t D_set;
  subr_t D_makunbound;
  subr_t D_fmakunbound;
  subr_t D_apply;
  subr_t D_funcall;
  subr_t D_mapcar;
  subr_t D_maplist;
  subr_t D_mapc;
  subr_t D_mapl;
  subr_t D_mapcan;
  subr_t D_mapcon;
  subr_t D_mapcap;
  subr_t D_maplap;
  subr_t D_values;
  subr_t D_values_list;
  subr_t D_driver;
  subr_t D_unwind_to_driver;
  subr_t D_macro_function;
  subr_t D_macroexpand;
  subr_t D_macroexpand_1;
  subr_t D_proclaim;
  subr_t D_eval;
  subr_t D_evalhook;
  subr_t D_applyhook;
  subr_t D_constantp;
  subr_t D_function_side_effect;
  subr_t D_function_name_p;
  subr_t D_parse_body;
  subr_t D_keyword_test;
  subr_t D_xor;
  subr_t D_read_form;
  subr_t D_read_eval_print;
  subr_t D_load;
  subr_t D_frame_up_1;
  subr_t D_frame_up;
  subr_t D_frame_down_1;
  subr_t D_frame_down;
  subr_t D_the_frame;
  subr_t D_same_env_as;
  subr_t D_eval_at;
  subr_t D_eval_frame_p;
  subr_t D_driver_frame_p;
  subr_t D_trap_eval_frame;
  subr_t D_redo_eval_frame;
  subr_t D_return_from_eval_frame;
  subr_t D_describe_frame;
  subr_t D_show_stack;
  subr_t D_debug;
  subr_t D_proom;
  subr_t D_gc;
  subr_t D_make_encoding;
  subr_t D_encodingp;
  subr_t D_charset_typep;
  subr_t D_encoding_line_terminator;
  subr_t D_encoding_charset;
  subr_t D_charset_range;
  subr_t D_default_file_encoding;
  subr_t D_set_default_file_encoding;
  subr_t D_pathname_encoding;
  subr_t D_set_pathname_encoding;
  subr_t D_terminal_encoding;
  subr_t D_set_terminal_encoding;
  subr_t D_misc_encoding;
  subr_t D_set_misc_encoding;
  subr_t D_convert_string_from_bytes;
  subr_t D_convert_string_to_bytes;
  subr_t D_error;
  subr_t D_defclcs;
  subr_t D_cerror_of_type;
  subr_t D_error_of_type;
  subr_t D_invoke_debugger;
  subr_t D_clcs_signal;
  subr_t D_make_hash_table;
  subr_t D_gethash;
  subr_t D_puthash;
  subr_t D_remhash;
  subr_t D_maphash;
  subr_t D_clrhash;
  subr_t D_hash_table_count;
  subr_t D_hash_table_rehash_size;
  subr_t D_hash_table_rehash_threshold;
  subr_t D_hash_table_size;
  subr_t D_hash_table_test;
  subr_t D_hash_table_iterator;
  subr_t D_hash_table_iterate;
  subr_t D_hash_table_weak_p;
  subr_t D_set_hash_table_weak_p;
  subr_t D_class_gethash;
  subr_t D_class_tuple_gethash;
  subr_t D_sxhash;
  subr_t D_defio;
  subr_t D_copy_readtable;
  subr_t D_set_syntax_from_char;
  subr_t D_set_macro_character;
  subr_t D_get_macro_character;
  subr_t D_make_dispatch_macro_character;
  subr_t D_set_dispatch_macro_character;
  subr_t D_get_dispatch_macro_character;
  subr_t D_readtable_case;
  subr_t D_set_readtable_case;
  subr_t D_lpar_reader;
  subr_t D_rpar_reader;
  subr_t D_string_reader;
  subr_t D_quote_reader;
  subr_t D_line_comment_reader;
  subr_t D_function_reader;
  subr_t D_comment_reader;
  subr_t D_char_reader;
  subr_t D_binary_reader;
  subr_t D_octal_reader;
  subr_t D_hexadecimal_reader;
  subr_t D_radix_reader;
  subr_t D_complex_reader;
  subr_t D_uninterned_reader;
  subr_t D_bit_vector_reader;
  subr_t D_vector_reader;
  subr_t D_array_reader;
  subr_t D_read_eval_reader;
  subr_t D_load_eval_reader;
  subr_t D_label_definition_reader;
  subr_t D_label_reference_reader;
  subr_t D_not_readable_reader;
  subr_t D_syntax_error_reader;
  subr_t D_feature_reader;
  subr_t D_not_feature_reader;
  subr_t D_structure_reader;
  subr_t D_closure_reader;
  subr_t D_clisp_pathname_reader;
  subr_t D_ansi_pathname_reader;
  subr_t D_unix_executable_reader;
  subr_t D_read;
  subr_t D_read_preserving_whitespace;
  subr_t D_read_delimited_list;
  subr_t D_read_line;
  subr_t D_read_char;
  subr_t D_unread_char;
  subr_t D_peek_char;
  subr_t D_listen;
  subr_t D_read_char_will_hang_p;
  subr_t D_read_char_no_hang;
  subr_t D_clear_input;
  subr_t D_read_from_string;
  subr_t D_parse_integer;
  subr_t D_print_structure;
  subr_t D_write;
  subr_t D_prin1;
  subr_t D_print;
  subr_t D_pprint;
  subr_t D_pprint_indent;
  subr_t D_pprint_newline;
  subr_t D_format_tabulate;
  subr_t D_ppprint_logical_block;
  subr_t D_pcirclep;
  subr_t D_princ;
  subr_t D_write_to_string;
  subr_t D_prin1_to_string;
  subr_t D_princ_to_string;
  subr_t D_write_char;
  subr_t D_write_string;
  subr_t D_write_line;
  subr_t D_terpri;
  subr_t D_fresh_line;
  subr_t D_finish_output;
  subr_t D_force_output;
  subr_t D_clear_output;
  subr_t D_write_unreadable;
  subr_t D_line_position;
  subr_t D_whitespacep;
  subr_t D_write_spaces;
  subr_t D_car;
  subr_t D_cdr;
  subr_t D_caar;
  subr_t D_cadr;
  subr_t D_cdar;
  subr_t D_cddr;
  subr_t D_caaar;
  subr_t D_caadr;
  subr_t D_cadar;
  subr_t D_caddr;
  subr_t D_cdaar;
  subr_t D_cdadr;
  subr_t D_cddar;
  subr_t D_cdddr;
  subr_t D_caaaar;
  subr_t D_caaadr;
  subr_t D_caadar;
  subr_t D_caaddr;
  subr_t D_cadaar;
  subr_t D_cadadr;
  subr_t D_caddar;
  subr_t D_cadddr;
  subr_t D_cdaaar;
  subr_t D_cdaadr;
  subr_t D_cdadar;
  subr_t D_cdaddr;
  subr_t D_cddaar;
  subr_t D_cddadr;
  subr_t D_cdddar;
  subr_t D_cddddr;
  subr_t D_cons;
  subr_t D_tree_equal;
  subr_t D_endp;
  subr_t D_list_length;
  subr_t D_list_length_dotted;
  subr_t D_nth;
  subr_t D_first;
  subr_t D_second;
  subr_t D_third;
  subr_t D_fourth;
  subr_t D_fifth;
  subr_t D_sixth;
  subr_t D_seventh;
  subr_t D_eighth;
  subr_t D_ninth;
  subr_t D_tenth;
  subr_t D_rest;
  subr_t D_nthcdr;
  subr_t D_last;
  subr_t D_list;
  subr_t D_liststern;
  subr_t D_make_list;
  subr_t D_append;
  subr_t D_copy_list;
  subr_t D_copy_alist;
  subr_t D_copy_tree;
  subr_t D_revappend;
  subr_t D_nconc;
  subr_t D_nreconc;
  subr_t D_list_nreverse;
  subr_t D_butlast;
  subr_t D_nbutlast;
  subr_t D_ldiff;
  subr_t D_rplaca;
  subr_t D_prplaca;
  subr_t D_rplacd;
  subr_t D_prplacd;
  subr_t D_subst;
  subr_t D_subst_if;
  subr_t D_subst_if_not;
  subr_t D_nsubst;
  subr_t D_nsubst_if;
  subr_t D_nsubst_if_not;
  subr_t D_sublis;
  subr_t D_nsublis;
  subr_t D_memq;
  subr_t D_member;
  subr_t D_member_if;
  subr_t D_member_if_not;
  subr_t D_tailp;
  subr_t D_adjoin;
  subr_t D_acons;
  subr_t D_pairlis;
  subr_t D_assoc;
  subr_t D_assoc_if;
  subr_t D_assoc_if_not;
  subr_t D_rassoc;
  subr_t D_rassoc_if;
  subr_t D_rassoc_if_not;
  subr_t D_list_upd;
  subr_t D_list_endtest;
  subr_t D_list_fe_init;
  subr_t D_list_access;
  subr_t D_list_access_set;
  subr_t D_list_llength;
  subr_t D_list_elt;
  subr_t D_list_set_elt;
  subr_t D_list_init_start;
  subr_t D_list_fe_init_end;
  subr_t D_lisp_implementation_type;
  subr_t D_lisp_implementation_version;
  subr_t D_version;
  subr_t D_machinetype;
  subr_t D_machine_version;
  subr_t D_get_env;
  subr_t D_set_env;
  subr_t D_software_type;
  subr_t D_software_version;
  subr_t D_identity;
  subr_t D_address_of;
  subr_t D_code_address_of;
  subr_t D_program_id;
  subr_t D_ansi;
  subr_t D_set_ansi;
  subr_t D_module_info;
  subr_t D_argv;
  subr_t D_current_language;
  subr_t D_set_current_language;
  subr_t D_text;
  subr_t D_i18n_gettext;
  subr_t D_i18n_ngettext;
  subr_t D_i18n_textdomain;
  subr_t D_i18n_set_textdomain;
  subr_t D_i18n_textdomaindir;
  subr_t D_i18n_set_textdomaindir;
  subr_t D_machine_instance;
  subr_t D_socket_service_port;
  subr_t D_get_internal_real_time;
  subr_t D_get_internal_run_time;
  subr_t D_get_universal_time;
  subr_t D_default_time_zone;
  subr_t D_sleep;
  subr_t D_time;
  subr_t D_delta4;
  subr_t D_make_symbol;
  subr_t D_find_package;
  subr_t D_pfind_package;
  subr_t D_package_name;
  subr_t D_package_nicknames;
  subr_t D_rename_package;
  subr_t D_package_use_list;
  subr_t D_package_used_by_list;
  subr_t D_package_shadowing_symbols;
  subr_t D_package_lock;
  subr_t D_package_case_sensitive_p;
  subr_t D_set_package_lock;
  subr_t D_symbol_value_lock;
  subr_t D_check_package_lock;
  subr_t D_list_all_packages;
  subr_t D_intern;
  subr_t D_find_symbol;
  subr_t D_unintern;
  subr_t D_export;
  subr_t D_unexport;
  subr_t D_re_export;
  subr_t D_import;
  subr_t D_shadowing_import;
  subr_t D_shadow;
  subr_t D_use_package;
  subr_t D_unuse_package;
  subr_t D_make_package;
  subr_t D_pin_package;
  subr_t D_delete_package;
  subr_t D_find_all_symbols;
  subr_t D_map_symbols;
  subr_t D_map_external_symbols;
  subr_t D_map_all_symbols;
  subr_t D_package_iterator;
  subr_t D_package_iterate;
  subr_t D_parse_namestring;
  subr_t D_pathname;
  subr_t D_pathnamehost;
  subr_t D_pathnamedevice;
  subr_t D_pathnamedirectory;
  subr_t D_pathnamename;
  subr_t D_pathnametype;
  subr_t D_pathnameversion;
  subr_t D_logical_pathname;
  subr_t D_translate_logical_pathname;
  subr_t D_file_namestring;
  subr_t D_directory_namestring;
  subr_t D_host_namestring;
  subr_t D_merge_pathnames;
  subr_t D_enough_namestring;
  subr_t D_make_pathname;
  subr_t D_make_logical_pathname;
  subr_t D_user_homedir_pathname;
  subr_t D_wild_pathname_p;
  subr_t D_pathname_match_p;
  subr_t D_translate_pathname;
  subr_t D_namestring;
  subr_t D_truename;
  subr_t D_probe_file;
  subr_t D_probe_directory;
  subr_t D_delete_file;
  subr_t D_rename_file;
  subr_t D_open;
  subr_t D_directory;
  subr_t D_cd;
  subr_t D_make_dir;
  subr_t D_delete_dir;
  subr_t D_ensure_directories_exist;
  subr_t D_file_write_date;
  subr_t D_file_author;
  subr_t D_execute;
  subr_t D_shell;
  subr_t D_launch;
  subr_t D_savemem;
  subr_t D_program_name;
  subr_t D_lib_directory;
  subr_t D_set_lib_directory;
  subr_t D_eq;
  subr_t D_eql;
  subr_t D_equal;
  subr_t D_equalp;
  subr_t D_consp;
  subr_t D_atom;
  subr_t D_symbolp;
  subr_t D_stringp;
  subr_t D_numberp;
  subr_t D_compiled_function_p;
  subr_t D_null;
  subr_t D_not;
  subr_t D_closurep;
  subr_t D_listp;
  subr_t D_integerp;
  subr_t D_fixnump;
  subr_t D_rationalp;
  subr_t D_floatp;
  subr_t D_short_float_p;
  subr_t D_single_float_p;
  subr_t D_double_float_p;
  subr_t D_long_float_p;
  subr_t D_realp;
  subr_t D_complexp;
  subr_t D_streamp;
  subr_t D_built_in_stream_p;
  subr_t D_random_state_p;
  subr_t D_readtablep;
  subr_t D_hash_table_p;
  subr_t D_pathnamep;
  subr_t D_logical_pathname_p;
  subr_t D_characterp;
  subr_t D_functionp;
  subr_t D_generic_function_p;
  subr_t D_packagep;
  subr_t D_arrayp;
  subr_t D_simple_array_p;
  subr_t D_bit_vector_p;
  subr_t D_vectorp;
  subr_t D_simple_vector_p;
  subr_t D_simple_string_p;
  subr_t D_simple_bit_vector_p;
  subr_t D_type_of;
  subr_t D_defclos;
  subr_t D_class_p;
  subr_t D_class_of;
  subr_t D_find_class;
  subr_t D_coerce;
  subr_t D_expand_deftype;
  subr_t D_note_new_structure_class;
  subr_t D_note_new_standard_class;
  subr_t D_heap_statistics;
  subr_t D_gc_statistics;
  subr_t D_list_statistics;
  subr_t D_heap_statistics_statistics;
  subr_t D_gc_statistics_statistics;
  subr_t D_record_ref;
  subr_t D_record_store;
  subr_t D_record_length;
  subr_t D_pstructure_ref;
  subr_t D_structure_ref;
  subr_t D_structure_store;
  subr_t D_make_structure;
  subr_t D_copy_structure;
  subr_t D_structure_type_p;
  subr_t D_closure_name;
  subr_t D_closure_codevec;
  subr_t D_closure_consts;
  subr_t D_make_code_vector;
  subr_t D_make_closure;
  subr_t D_closure_set_seclass;
  subr_t D_copy_generic_function;
  subr_t D_generic_function_effective_method_function;
  subr_t D_make_load_time_eval;
  subr_t D_make_symbol_macro;
  subr_t D_symbol_macro_p;
  subr_t D_symbol_macro_expand;
  subr_t D_make_macro;
  subr_t D_macrop;
  subr_t D_macro_expander;
  subr_t D_make_function_macro;
  subr_t D_function_macro_p;
  subr_t D_function_macro_function;
  subr_t D_function_macro_expander;
  subr_t D_make_weak_pointer;
  subr_t D_weak_pointer_p;
  subr_t D_weak_pointer_value;
  subr_t D_set_weak_pointer_value;
  subr_t D_finalize;
  subr_t D_structure_object_p;
  subr_t D_std_instance_p;
  subr_t D_allocate_std_instance;
  subr_t D_pallocate_instance;
  subr_t D_slot_value;
  subr_t D_set_slot_value;
  subr_t D_slot_boundp;
  subr_t D_slot_makunbound;
  subr_t D_slot_exists_p;
  subr_t D_pshared_initialize;
  subr_t D_preinitialize_instance;
  subr_t D_pinitialize_instance;
  subr_t D_pmake_instance;
  subr_t D_pchange_class;
  subr_t D_sequencep;
  subr_t D_defseq;
  subr_t D_elt;
  subr_t D_setelt;
  subr_t D_subseq;
  subr_t D_copy_seq;
  subr_t D_length;
  subr_t D_reverse;
  subr_t D_nreverse;
  subr_t D_make_sequence;
  subr_t D_coerced_subseq;
  subr_t D_concatenate;
  subr_t D_map;
  subr_t D_map_into;
  subr_t D_some;
  subr_t D_every;
  subr_t D_notany;
  subr_t D_notevery;
  subr_t D_reduce;
  subr_t D_fill;
  subr_t D_replace;
  subr_t D_remove;
  subr_t D_remove_if;
  subr_t D_remove_if_not;
  subr_t D_delete;
  subr_t D_delete_if;
  subr_t D_delete_if_not;
  subr_t D_remove_duplicates;
  subr_t D_delete_duplicates;
  subr_t D_substitute;
  subr_t D_substitute_if;
  subr_t D_substitute_if_not;
  subr_t D_nsubstitute;
  subr_t D_nsubstitute_if;
  subr_t D_nsubstitute_if_not;
  subr_t D_find;
  subr_t D_find_if;
  subr_t D_find_if_not;
  subr_t D_position;
  subr_t D_position_if;
  subr_t D_position_if_not;
  subr_t D_count;
  subr_t D_count_if;
  subr_t D_count_if_not;
  subr_t D_mismatch;
  subr_t D_search;
  subr_t D_sort;
  subr_t D_stable_sort;
  subr_t D_merge;
  subr_t D_read_char_sequence;
  subr_t D_write_char_sequence;
  subr_t D_read_byte_sequence;
  subr_t D_write_byte_sequence;
  subr_t D_symbol_stream;
  subr_t D_make_synonym_stream;
  subr_t D_synonym_stream_p;
  subr_t D_synonym_stream_symbol;
  subr_t D_make_broadcast_stream;
  subr_t D_broadcast_stream_p;
  subr_t D_broadcast_stream_streams;
  subr_t D_make_concatenated_stream;
  subr_t D_concatenated_stream_p;
  subr_t D_concatenated_stream_streams;
  subr_t D_make_two_way_stream;
  subr_t D_two_way_stream_p;
  subr_t D_two_way_stream_input_stream;
  subr_t D_two_way_stream_output_stream;
  subr_t D_make_echo_stream;
  subr_t D_echo_stream_p;
  subr_t D_echo_stream_input_stream;
  subr_t D_echo_stream_output_stream;
  subr_t D_make_string_input_stream;
  subr_t D_string_input_stream_index;
  subr_t D_make_string_output_stream;
  subr_t D_get_output_stream_string;
  subr_t D_make_string_push_stream;
  subr_t D_string_stream_p;
  subr_t D_make_buffered_input_stream;
  subr_t D_buffered_input_stream_index;
  subr_t D_make_buffered_output_stream;
  subr_t D_generic_stream_controller;
  subr_t D_make_generic_stream;
  subr_t D_generic_stream_p;
  subr_t D_make_keyboard_stream;
  subr_t D_terminal_raw;
  subr_t D_make_window;
  subr_t D_window_size;
  subr_t D_window_cursor_position;
  subr_t D_set_window_cursor_position;
  subr_t D_clear_window;
  subr_t D_clear_window_to_eot;
  subr_t D_clear_window_to_eol;
  subr_t D_delete_window_line;
  subr_t D_insert_window_line;
  subr_t D_highlight_on;
  subr_t D_highlight_off;
  subr_t D_window_cursor_on;
  subr_t D_window_cursor_off;
  subr_t D_file_stream_p;
  subr_t D_make_pipe_input_stream;
  subr_t D_make_pipe_output_stream;
  subr_t D_make_pipe_io_stream;
  subr_t D_make_x11socket_stream;
  subr_t D_read_n_bytes;
  subr_t D_write_n_bytes;
  subr_t D_socket_server_close;
  subr_t D_socket_server;
  subr_t D_socket_server_port;
  subr_t D_socket_server_host;
  subr_t D_socket_accept;
  subr_t D_socket_wait;
  subr_t D_socket_status;
  subr_t D_socket_connect;
  subr_t D_socket_stream_port;
  subr_t D_socket_stream_host;
  subr_t D_socket_stream_peer;
  subr_t D_socket_stream_local;
  subr_t D_socket_options;
  subr_t D_socket_stream_shutdown;
  subr_t D_make_stream;
  subr_t D_socket_stream_handle;
  subr_t D_built_in_stream_open_p;
  subr_t D_input_stream_p;
  subr_t D_output_stream_p;
  subr_t D_stream_element_type_eq;
  subr_t D_built_in_stream_element_type;
  subr_t D_built_in_stream_set_element_type;
  subr_t D_stream_external_format;
  subr_t D_set_stream_external_format;
  subr_t D_interactive_stream_p;
  subr_t D_built_in_stream_close;
  subr_t D_read_byte;
  subr_t D_read_byte_lookahead;
  subr_t D_read_byte_will_hang_p;
  subr_t D_read_byte_no_hang;
  subr_t D_read_integer;
  subr_t D_read_float;
  subr_t D_write_byte;
  subr_t D_write_integer;
  subr_t D_write_float;
  subr_t D_file_position;
  subr_t D_file_length;
  subr_t D_file_string_length;
  subr_t D_line_number;
  subr_t D_allow_read_eval;
  subr_t D_defgray;
  subr_t D_putd;
  subr_t D_find_subr;
  subr_t D_proclaim_constant;
  subr_t D_get;
  subr_t D_getf;
  subr_t D_putf;
  subr_t D_remf;
  subr_t D_get_properties;
  subr_t D_putplist;
  subr_t D_put;
  subr_t D_remprop;
  subr_t D_symbol_package;
  subr_t D_symbol_plist;
  subr_t D_symbol_name;
  subr_t D_keywordp;
  subr_t D_gensym;
  subr_t D_decimal_string;
  subr_t D_zerop;
  subr_t D_plusp;
  subr_t D_minusp;
  subr_t D_oddp;
  subr_t D_evenp;
  subr_t D_gleich;
  subr_t D_ungleich;
  subr_t D_kleiner;
  subr_t D_groesser;
  subr_t D_klgleich;
  subr_t D_grgleich;
  subr_t D_max;
  subr_t D_min;
  subr_t D_plus;
  subr_t D_minus;
  subr_t D_mal;
  subr_t D_durch;
  subr_t D_einsplus;
  subr_t D_einsminus;
  subr_t D_conjugate;
  subr_t D_gcd;
  subr_t D_xgcd;
  subr_t D_lcm;
  subr_t D_exp;
  subr_t D_expt;
  subr_t D_log;
  subr_t D_sqrt;
  subr_t D_isqrt;
  subr_t D_abs;
  subr_t D_phase;
  subr_t D_signum;
  subr_t D_sin;
  subr_t D_cos;
  subr_t D_tan;
  subr_t D_cis;
  subr_t D_asin;
  subr_t D_acos;
  subr_t D_atan;
  subr_t D_sinh;
  subr_t D_cosh;
  subr_t D_tanh;
  subr_t D_asinh;
  subr_t D_acosh;
  subr_t D_atanh;
  subr_t D_float;
  subr_t D_rational;
  subr_t D_rationalize;
  subr_t D_numerator;
  subr_t D_denominator;
  subr_t D_floor;
  subr_t D_ceiling;
  subr_t D_truncate;
  subr_t D_round;
  subr_t D_mod;
  subr_t D_rem;
  subr_t D_ffloor;
  subr_t D_fceiling;
  subr_t D_ftruncate;
  subr_t D_fround;
  subr_t D_decode_float;
  subr_t D_scale_float;
  subr_t D_float_radix;
  subr_t D_float_sign;
  subr_t D_float_digits;
  subr_t D_float_precision;
  subr_t D_integer_decode_float;
  subr_t D_complex;
  subr_t D_realpart;
  subr_t D_imagpart;
  subr_t D_logior;
  subr_t D_logxor;
  subr_t D_logand;
  subr_t D_logeqv;
  subr_t D_lognand;
  subr_t D_lognor;
  subr_t D_logandc1;
  subr_t D_logandc2;
  subr_t D_logorc1;
  subr_t D_logorc2;
  subr_t D_boole;
  subr_t D_lognot;
  subr_t D_logtest;
  subr_t D_logbitp;
  subr_t D_ash;
  subr_t D_logcount;
  subr_t D_integer_length;
  subr_t D_byte;
  subr_t D_bytesize;
  subr_t D_byteposition;
  subr_t D_ldb;
  subr_t D_ldb_test;
  subr_t D_mask_field;
  subr_t D_dpb;
  subr_t D_deposit_field;
  subr_t D_random;
  subr_t D_make_random_state;
  subr_t D_fakultaet;
  subr_t D_exquo;
  subr_t D_mod_expt;
  subr_t D_long_float_digits;
  subr_t D_set_long_float_digits;
  subr_t D_log2;
  subr_t D_log10;
} subr_tab_data;
#define subr_tab  subr_tab_data
#define subr_tab_ptr_as_object(subr_addr)  objectplus(subr_addr,2UL)
#define L(name)  subr_tab_ptr_as_object(&subr_tab.D_##name)
extern struct symbol_tab_ {
  VAROBJECTS_ALIGNMENT_DUMMY_DECL
  symbol_ S_nil;
  symbol_ S_t;
  symbol_ S_eval_when;
  symbol_ S_quote;
  symbol_ S_function;
  symbol_ S_setq;
  symbol_ S_psetq;
  symbol_ S_progn;
  symbol_ S_prog1;
  symbol_ S_prog2;
  symbol_ S_let;
  symbol_ S_letstern;
  symbol_ S_locally;
  symbol_ S_compiler_let;
  symbol_ S_progv;
  symbol_ S_flet;
  symbol_ S_labels;
  symbol_ S_macrolet;
  symbol_ S_function_macro_let;
  symbol_ S_symbol_macrolet;
  symbol_ S_if;
  symbol_ S_when;
  symbol_ S_unless;
  symbol_ S_cond;
  symbol_ S_case;
  symbol_ S_block;
  symbol_ S_return_from;
  symbol_ S_tagbody;
  symbol_ S_go;
  symbol_ S_multiple_value_list;
  symbol_ S_multiple_value_call;
  symbol_ S_multiple_value_prog1;
  symbol_ S_multiple_value_bind;
  symbol_ S_multiple_value_setq;
  symbol_ S_catch;
  symbol_ S_unwind_protect;
  symbol_ S_throw;
  symbol_ S_declare;
  symbol_ S_the;
  symbol_ S_load_time_value;
  symbol_ S_and;
  symbol_ S_or;
  symbol_ S_funtabref;
  symbol_ S_subr_info;
  symbol_ S_add_implicit_block;
  symbol_ S_function_block_name;
  symbol_ S_copy_simple_vector;
  symbol_ S_vector;
  symbol_ S_aref;
  symbol_ S_store;
  symbol_ S_svref;
  symbol_ S_svstore;
  symbol_ S_psvstore;
  symbol_ S_row_major_aref;
  symbol_ S_row_major_store;
  symbol_ S_array_element_type;
  symbol_ S_array_rank;
  symbol_ S_array_dimension;
  symbol_ S_array_dimensions;
  symbol_ S_array_total_size;
  symbol_ S_array_in_bounds_p;
  symbol_ S_array_row_major_index;
  symbol_ S_adjustable_array_p;
  symbol_ S_array_displacement;
  symbol_ S_bit;
  symbol_ S_sbit;
  symbol_ S_bit_and;
  symbol_ S_bit_ior;
  symbol_ S_bit_xor;
  symbol_ S_bit_eqv;
  symbol_ S_bit_nand;
  symbol_ S_bit_nor;
  symbol_ S_bit_andc1;
  symbol_ S_bit_andc2;
  symbol_ S_bit_orc1;
  symbol_ S_bit_orc2;
  symbol_ S_bit_not;
  symbol_ S_array_has_fill_pointer_p;
  symbol_ S_fill_pointer;
  symbol_ S_set_fill_pointer;
  symbol_ S_vector_push;
  symbol_ S_vector_pop;
  symbol_ S_vector_push_extend;
  symbol_ S_make_array;
  symbol_ S_adjust_array;
  symbol_ S_vector_init;
  symbol_ S_vector_upd;
  symbol_ S_vector_endtest;
  symbol_ S_vector_fe_init;
  symbol_ S_vector_fe_upd;
  symbol_ S_vector_fe_endtest;
  symbol_ S_vector_length;
  symbol_ S_vector_init_start;
  symbol_ S_vector_fe_init_end;
  symbol_ S_make_bit_vector;
  symbol_ S_string_info;
  symbol_ S_standard_char_p;
  symbol_ S_graphic_char_p;
  symbol_ S_char_width;
  symbol_ S_string_char_p;
  symbol_ S_alpha_char_p;
  symbol_ S_upper_case_p;
  symbol_ S_lower_case_p;
  symbol_ S_both_case_p;
  symbol_ S_digit_char_p;
  symbol_ S_alphanumericp;
  symbol_ S_char_gleich;
  symbol_ S_char_ungleich;
  symbol_ S_char_kleiner;
  symbol_ S_char_groesser;
  symbol_ S_char_klgleich;
  symbol_ S_char_grgleich;
  symbol_ S_char_equal;
  symbol_ S_char_not_equal;
  symbol_ S_char_lessp;
  symbol_ S_char_greaterp;
  symbol_ S_char_not_greaterp;
  symbol_ S_char_not_lessp;
  symbol_ S_char_code;
  symbol_ S_code_char;
  symbol_ S_character;
  symbol_ S_char_upcase;
  symbol_ S_char_downcase;
  symbol_ S_digit_char;
  symbol_ S_char_int;
  symbol_ S_int_char;
  symbol_ S_char_name;
  symbol_ S_char;
  symbol_ S_schar;
  symbol_ S_store_char;
  symbol_ S_store_schar;
  symbol_ S_string_gleich;
  symbol_ S_string_ungleich;
  symbol_ S_string_kleiner;
  symbol_ S_string_groesser;
  symbol_ S_string_klgleich;
  symbol_ S_string_grgleich;
  symbol_ S_string_equal;
  symbol_ S_string_not_equal;
  symbol_ S_string_lessp;
  symbol_ S_string_greaterp;
  symbol_ S_string_not_greaterp;
  symbol_ S_string_not_lessp;
  symbol_ S_search_string_gleich;
  symbol_ S_search_string_equal;
  symbol_ S_make_string;
  symbol_ S_string_both_trim;
  symbol_ S_string_width;
  symbol_ S_nstring_upcase;
  symbol_ S_string_upcase;
  symbol_ S_nstring_downcase;
  symbol_ S_string_downcase;
  symbol_ S_nstring_capitalize;
  symbol_ S_string_capitalize;
  symbol_ S_string;
  symbol_ S_name_char;
  symbol_ S_substring;
  symbol_ S_string_concat;
  symbol_ S_exit;
  symbol_ S_psymbol_value;
  symbol_ S_symbol_value;
  symbol_ S_symbol_function;
  symbol_ S_fdefinition;
  symbol_ S_boundp;
  symbol_ S_fboundp;
  symbol_ S_special_operator_p;
  symbol_ S_set;
  symbol_ S_makunbound;
  symbol_ S_fmakunbound;
  symbol_ S_apply;
  symbol_ S_funcall;
  symbol_ S_mapcar;
  symbol_ S_maplist;
  symbol_ S_mapc;
  symbol_ S_mapl;
  symbol_ S_mapcan;
  symbol_ S_mapcon;
  symbol_ S_mapcap;
  symbol_ S_maplap;
  symbol_ S_values;
  symbol_ S_values_list;
  symbol_ S_driver;
  symbol_ S_unwind_to_driver;
  symbol_ S_macro_function;
  symbol_ S_macroexpand;
  symbol_ S_macroexpand_1;
  symbol_ S_proclaim;
  symbol_ S_eval;
  symbol_ S_evalhook;
  symbol_ S_applyhook;
  symbol_ S_constantp;
  symbol_ S_function_side_effect;
  symbol_ S_function_name_p;
  symbol_ S_parse_body;
  symbol_ S_keyword_test;
  symbol_ S_xor;
  symbol_ S_read_form;
  symbol_ S_read_eval_print;
  symbol_ S_load;
  symbol_ S_frame_up_1;
  symbol_ S_frame_up;
  symbol_ S_frame_down_1;
  symbol_ S_frame_down;
  symbol_ S_the_frame;
  symbol_ S_same_env_as;
  symbol_ S_eval_at;
  symbol_ S_eval_frame_p;
  symbol_ S_driver_frame_p;
  symbol_ S_trap_eval_frame;
  symbol_ S_redo_eval_frame;
  symbol_ S_return_from_eval_frame;
  symbol_ S_describe_frame;
  symbol_ S_show_stack;
  symbol_ S_debug;
  symbol_ S_proom;
  symbol_ S_gc;
  symbol_ S_make_encoding;
  symbol_ S_encodingp;
  symbol_ S_charset_typep;
  symbol_ S_encoding_line_terminator;
  symbol_ S_encoding_charset;
  symbol_ S_charset_range;
  symbol_ S_default_file_encoding;
  symbol_ S_set_default_file_encoding;
  symbol_ S_pathname_encoding;
  symbol_ S_set_pathname_encoding;
  symbol_ S_terminal_encoding;
  symbol_ S_set_terminal_encoding;
  symbol_ S_misc_encoding;
  symbol_ S_set_misc_encoding;
  symbol_ S_convert_string_from_bytes;
  symbol_ S_convert_string_to_bytes;
  symbol_ S_error;
  symbol_ S_defclcs;
  symbol_ S_cerror_of_type;
  symbol_ S_error_of_type;
  symbol_ S_invoke_debugger;
  symbol_ S_clcs_signal;
  symbol_ S_make_hash_table;
  symbol_ S_gethash;
  symbol_ S_puthash;
  symbol_ S_remhash;
  symbol_ S_maphash;
  symbol_ S_clrhash;
  symbol_ S_hash_table_count;
  symbol_ S_hash_table_rehash_size;
  symbol_ S_hash_table_rehash_threshold;
  symbol_ S_hash_table_size;
  symbol_ S_hash_table_test;
  symbol_ S_hash_table_iterator;
  symbol_ S_hash_table_iterate;
  symbol_ S_hash_table_weak_p;
  symbol_ S_set_hash_table_weak_p;
  symbol_ S_class_gethash;
  symbol_ S_class_tuple_gethash;
  symbol_ S_sxhash;
  symbol_ S_defio;
  symbol_ S_copy_readtable;
  symbol_ S_set_syntax_from_char;
  symbol_ S_set_macro_character;
  symbol_ S_get_macro_character;
  symbol_ S_make_dispatch_macro_character;
  symbol_ S_set_dispatch_macro_character;
  symbol_ S_get_dispatch_macro_character;
  symbol_ S_readtable_case;
  symbol_ S_set_readtable_case;
  symbol_ S_lpar_reader;
  symbol_ S_rpar_reader;
  symbol_ S_quote_reader;
  symbol_ S_function_reader;
  symbol_ S_string_reader;
  symbol_ S_line_comment_reader;
  symbol_ S_comment_reader;
  symbol_ S_char_reader;
  symbol_ S_binary_reader;
  symbol_ S_octal_reader;
  symbol_ S_hexadecimal_reader;
  symbol_ S_radix_reader;
  symbol_ S_complex_reader;
  symbol_ S_uninterned_reader;
  symbol_ S_bit_vector_reader;
  symbol_ S_vector_reader;
  symbol_ S_array_reader;
  symbol_ S_read_eval_reader;
  symbol_ S_load_eval_reader;
  symbol_ S_label_definition_reader;
  symbol_ S_label_reference_reader;
  symbol_ S_not_readable_reader;
  symbol_ S_syntax_error_reader;
  symbol_ S_feature_reader;
  symbol_ S_not_feature_reader;
  symbol_ S_structure_reader;
  symbol_ S_closure_reader;
  symbol_ S_clisp_pathname_reader;
  symbol_ S_ansi_pathname_reader;
  symbol_ S_unix_executable_reader;
  symbol_ S_read;
  symbol_ S_read_preserving_whitespace;
  symbol_ S_read_delimited_list;
  symbol_ S_read_line;
  symbol_ S_read_char;
  symbol_ S_unread_char;
  symbol_ S_peek_char;
  symbol_ S_listen;
  symbol_ S_read_char_will_hang_p;
  symbol_ S_read_char_no_hang;
  symbol_ S_clear_input;
  symbol_ S_read_from_string;
  symbol_ S_parse_integer;
  symbol_ S_print_structure;
  symbol_ S_write;
  symbol_ S_prin1;
  symbol_ S_print;
  symbol_ S_pprint;
  symbol_ S_pprint_dispatch;
  symbol_ S_pprint_indent;
  symbol_ S_pprint_newline;
  symbol_ S_ppprint_logical_block;
  symbol_ S_pcirclep;
  symbol_ S_princ;
  symbol_ S_write_to_string;
  symbol_ S_prin1_to_string;
  symbol_ S_princ_to_string;
  symbol_ S_write_char;
  symbol_ S_write_string;
  symbol_ S_write_line;
  symbol_ S_terpri;
  symbol_ S_fresh_line;
  symbol_ S_finish_output;
  symbol_ S_force_output;
  symbol_ S_clear_output;
  symbol_ S_write_unreadable;
  symbol_ S_line_position;
  symbol_ S_whitespacep;
  symbol_ S_write_spaces;
  symbol_ S_car;
  symbol_ S_cdr;
  symbol_ S_caar;
  symbol_ S_cadr;
  symbol_ S_cdar;
  symbol_ S_cddr;
  symbol_ S_caaar;
  symbol_ S_caadr;
  symbol_ S_cadar;
  symbol_ S_caddr;
  symbol_ S_cdaar;
  symbol_ S_cdadr;
  symbol_ S_cddar;
  symbol_ S_cdddr;
  symbol_ S_caaaar;
  symbol_ S_caaadr;
  symbol_ S_caadar;
  symbol_ S_caaddr;
  symbol_ S_cadaar;
  symbol_ S_cadadr;
  symbol_ S_caddar;
  symbol_ S_cadddr;
  symbol_ S_cdaaar;
  symbol_ S_cdaadr;
  symbol_ S_cdadar;
  symbol_ S_cdaddr;
  symbol_ S_cddaar;
  symbol_ S_cddadr;
  symbol_ S_cdddar;
  symbol_ S_cddddr;
  symbol_ S_cons;
  symbol_ S_tree_equal;
  symbol_ S_endp;
  symbol_ S_list_length;
  symbol_ S_list_length_dotted;
  symbol_ S_nth;
  symbol_ S_first;
  symbol_ S_second;
  symbol_ S_third;
  symbol_ S_fourth;
  symbol_ S_fifth;
  symbol_ S_sixth;
  symbol_ S_seventh;
  symbol_ S_eighth;
  symbol_ S_ninth;
  symbol_ S_tenth;
  symbol_ S_rest;
  symbol_ S_nthcdr;
  symbol_ S_last;
  symbol_ S_list;
  symbol_ S_liststern;
  symbol_ S_make_list;
  symbol_ S_append;
  symbol_ S_copy_list;
  symbol_ S_copy_alist;
  symbol_ S_copy_tree;
  symbol_ S_revappend;
  symbol_ S_nconc;
  symbol_ S_nreconc;
  symbol_ S_list_nreverse;
  symbol_ S_butlast;
  symbol_ S_nbutlast;
  symbol_ S_ldiff;
  symbol_ S_rplaca;
  symbol_ S_prplaca;
  symbol_ S_rplacd;
  symbol_ S_prplacd;
  symbol_ S_subst;
  symbol_ S_subst_if;
  symbol_ S_subst_if_not;
  symbol_ S_nsubst;
  symbol_ S_nsubst_if;
  symbol_ S_nsubst_if_not;
  symbol_ S_sublis;
  symbol_ S_nsublis;
  symbol_ S_memq;
  symbol_ S_member;
  symbol_ S_member_if;
  symbol_ S_member_if_not;
  symbol_ S_tailp;
  symbol_ S_adjoin;
  symbol_ S_acons;
  symbol_ S_pairlis;
  symbol_ S_assoc;
  symbol_ S_assoc_if;
  symbol_ S_assoc_if_not;
  symbol_ S_rassoc;
  symbol_ S_rassoc_if;
  symbol_ S_rassoc_if_not;
  symbol_ S_list_upd;
  symbol_ S_list_endtest;
  symbol_ S_list_fe_init;
  symbol_ S_list_access;
  symbol_ S_list_access_set;
  symbol_ S_list_llength;
  symbol_ S_list_elt;
  symbol_ S_list_set_elt;
  symbol_ S_list_init_start;
  symbol_ S_list_fe_init_end;
  symbol_ S_lisp_implementation_type;
  symbol_ S_lisp_implementation_version;
  symbol_ S_version;
  symbol_ S_machinetype;
  symbol_ S_machine_version;
  symbol_ S_get_env;
  symbol_ S_set_env;
  symbol_ S_software_type;
  symbol_ S_software_version;
  symbol_ S_identity;
  symbol_ S_address_of;
  symbol_ S_code_address_of;
  symbol_ S_program_id;
  symbol_ S_ansi;
  symbol_ S_set_ansi;
  symbol_ S_module_info;
  symbol_ S_argv;
  symbol_ S_current_language;
  symbol_ S_set_current_language;
  symbol_ S_text;
  symbol_ S_i18n_gettext;
  symbol_ S_i18n_ngettext;
  symbol_ S_i18n_textdomain;
  symbol_ S_i18n_set_textdomain;
  symbol_ S_i18n_textdomaindir;
  symbol_ S_i18n_set_textdomaindir;
  symbol_ S_machine_instance;
  symbol_ S_socket_service_port;
  symbol_ S_get_internal_real_time;
  symbol_ S_get_internal_run_time;
  symbol_ S_get_universal_time;
  symbol_ S_default_time_zone;
  symbol_ S_sleep;
  symbol_ S_time;
  symbol_ S_delta4;
  symbol_ S_make_symbol;
  symbol_ S_find_package;
  symbol_ S_pfind_package;
  symbol_ S_package_name;
  symbol_ S_package_nicknames;
  symbol_ S_rename_package;
  symbol_ S_package_use_list;
  symbol_ S_package_used_by_list;
  symbol_ S_package_shadowing_symbols;
  symbol_ S_package_lock;
  symbol_ S_package_case_sensitive_p;
  symbol_ S_set_package_lock;
  symbol_ S_symbol_value_lock;
  symbol_ S_check_package_lock;
  symbol_ S_list_all_packages;
  symbol_ S_intern;
  symbol_ S_find_symbol;
  symbol_ S_unintern;
  symbol_ S_export;
  symbol_ S_unexport;
  symbol_ S_re_export;
  symbol_ S_import;
  symbol_ S_shadowing_import;
  symbol_ S_shadow;
  symbol_ S_use_package;
  symbol_ S_unuse_package;
  symbol_ S_make_package;
  symbol_ S_pin_package;
  symbol_ S_delete_package;
  symbol_ S_find_all_symbols;
  symbol_ S_map_symbols;
  symbol_ S_map_external_symbols;
  symbol_ S_map_all_symbols;
  symbol_ S_package_iterator;
  symbol_ S_package_iterate;
  symbol_ S_parse_namestring;
  symbol_ S_pathname;
  symbol_ S_pathnamehost;
  symbol_ S_pathnamedevice;
  symbol_ S_pathnamedirectory;
  symbol_ S_pathnamename;
  symbol_ S_pathnametype;
  symbol_ S_pathnameversion;
  symbol_ S_logical_pathname;
  symbol_ S_translate_logical_pathname;
  symbol_ S_file_namestring;
  symbol_ S_directory_namestring;
  symbol_ S_host_namestring;
  symbol_ S_merge_pathnames;
  symbol_ S_enough_namestring;
  symbol_ S_make_pathname;
  symbol_ S_make_logical_pathname;
  symbol_ S_user_homedir_pathname;
  symbol_ S_wild_pathname_p;
  symbol_ S_pathname_match_p;
  symbol_ S_translate_pathname;
  symbol_ S_namestring;
  symbol_ S_truename;
  symbol_ S_probe_file;
  symbol_ S_probe_directory;
  symbol_ S_delete_file;
  symbol_ S_rename_file;
  symbol_ S_open;
  symbol_ S_directory;
  symbol_ S_cd;
  symbol_ S_make_dir;
  symbol_ S_delete_dir;
  symbol_ S_ensure_directories_exist;
  symbol_ S_file_write_date;
  symbol_ S_file_author;
  symbol_ S_execute;
  symbol_ S_shell;
  symbol_ S_launch;
  symbol_ S_savemem;
  symbol_ S_program_name;
  symbol_ S_lib_directory;
  symbol_ S_set_lib_directory;
  symbol_ S_eq;
  symbol_ S_eql;
  symbol_ S_equal;
  symbol_ S_equalp;
  symbol_ S_consp;
  symbol_ S_atom;
  symbol_ S_symbolp;
  symbol_ S_stringp;
  symbol_ S_numberp;
  symbol_ S_compiled_function_p;
  symbol_ S_null;
  symbol_ S_not;
  symbol_ S_closurep;
  symbol_ S_listp;
  symbol_ S_integerp;
  symbol_ S_fixnump;
  symbol_ S_rationalp;
  symbol_ S_floatp;
  symbol_ S_short_float_p;
  symbol_ S_single_float_p;
  symbol_ S_double_float_p;
  symbol_ S_long_float_p;
  symbol_ S_realp;
  symbol_ S_complexp;
  symbol_ S_streamp;
  symbol_ S_built_in_stream_p;
  symbol_ S_random_state_p;
  symbol_ S_readtablep;
  symbol_ S_hash_table_p;
  symbol_ S_pathnamep;
  symbol_ S_logical_pathname_p;
  symbol_ S_characterp;
  symbol_ S_functionp;
  symbol_ S_generic_function_p;
  symbol_ S_packagep;
  symbol_ S_arrayp;
  symbol_ S_simple_array_p;
  symbol_ S_bit_vector_p;
  symbol_ S_vectorp;
  symbol_ S_simple_vector_p;
  symbol_ S_simple_string_p;
  symbol_ S_simple_bit_vector_p;
  symbol_ S_type_of;
  symbol_ S_defclos;
  symbol_ S_class_p;
  symbol_ S_class_of;
  symbol_ S_find_class;
  symbol_ S_coerce;
  symbol_ S_coerce_fixnum_char_ansi;
  symbol_ S_note_new_structure_class;
  symbol_ S_note_new_standard_class;
  symbol_ S_heap_statistics;
  symbol_ S_gc_statistics;
  symbol_ S_list_statistics;
  symbol_ S_heap_statistics_statistics;
  symbol_ S_gc_statistics_statistics;
  symbol_ S_record_ref;
  symbol_ S_record_store;
  symbol_ S_record_length;
  symbol_ S_pstructure_ref;
  symbol_ S_structure_ref;
  symbol_ S_structure_store;
  symbol_ S_make_structure;
  symbol_ S_copy_structure;
  symbol_ S_structure_type_p;
  symbol_ S_closure_name;
  symbol_ S_closure_codevec;
  symbol_ S_closure_consts;
  symbol_ S_make_code_vector;
  symbol_ S_make_closure;
  symbol_ S_closure_set_seclass;
  symbol_ S_copy_generic_function;
  symbol_ S_generic_function_effective_method_function;
  symbol_ S_make_load_time_eval;
  symbol_ S_make_symbol_macro;
  symbol_ S_symbol_macro_p;
  symbol_ S_symbol_macro_expand;
  symbol_ S_make_macro;
  symbol_ S_macrop;
  symbol_ S_macro_expander;
  symbol_ S_make_function_macro;
  symbol_ S_function_macro_p;
  symbol_ S_function_macro_function;
  symbol_ S_function_macro_expander;
  symbol_ S_make_weak_pointer;
  symbol_ S_weak_pointer_p;
  symbol_ S_weak_pointer_value;
  symbol_ S_set_weak_pointer_value;
  symbol_ S_finalize;
  symbol_ S_structure_object_p;
  symbol_ S_std_instance_p;
  symbol_ S_allocate_std_instance;
  symbol_ S_class_finalize;
  symbol_ S_pallocate_instance;
  symbol_ S_slot_value;
  symbol_ S_set_slot_value;
  symbol_ S_slot_boundp;
  symbol_ S_slot_makunbound;
  symbol_ S_slot_exists_p;
  symbol_ S_pshared_initialize;
  symbol_ S_preinitialize_instance;
  symbol_ S_pinitialize_instance;
  symbol_ S_pmake_instance;
  symbol_ S_pchange_class;
  symbol_ S_make_instance;
  symbol_ S_shared_initialize;
  symbol_ S_reinitialize_instance;
  symbol_ S_initialize_instance;
  symbol_ S_update_instance_frc;
  symbol_ S_sequencep;
  symbol_ S_defseq;
  symbol_ S_elt;
  symbol_ S_setelt;
  symbol_ S_subseq;
  symbol_ S_copy_seq;
  symbol_ S_length;
  symbol_ S_reverse;
  symbol_ S_nreverse;
  symbol_ S_make_sequence;
  symbol_ S_coerced_subseq;
  symbol_ S_concatenate;
  symbol_ S_map;
  symbol_ S_map_into;
  symbol_ S_some;
  symbol_ S_every;
  symbol_ S_notany;
  symbol_ S_notevery;
  symbol_ S_reduce;
  symbol_ S_fill;
  symbol_ S_replace;
  symbol_ S_remove;
  symbol_ S_remove_if;
  symbol_ S_remove_if_not;
  symbol_ S_delete;
  symbol_ S_delete_if;
  symbol_ S_delete_if_not;
  symbol_ S_remove_duplicates;
  symbol_ S_delete_duplicates;
  symbol_ S_substitute;
  symbol_ S_substitute_if;
  symbol_ S_substitute_if_not;
  symbol_ S_nsubstitute;
  symbol_ S_nsubstitute_if;
  symbol_ S_nsubstitute_if_not;
  symbol_ S_find;
  symbol_ S_find_if;
  symbol_ S_find_if_not;
  symbol_ S_position;
  symbol_ S_position_if;
  symbol_ S_position_if_not;
  symbol_ S_count;
  symbol_ S_count_if;
  symbol_ S_count_if_not;
  symbol_ S_mismatch;
  symbol_ S_search;
  symbol_ S_sort;
  symbol_ S_stable_sort;
  symbol_ S_merge;
  symbol_ S_read_char_sequence;
  symbol_ S_write_char_sequence;
  symbol_ S_read_byte_sequence;
  symbol_ S_write_byte_sequence;
  symbol_ S_sequence_count_ansi;
  symbol_ S_symbol_stream;
  symbol_ S_make_synonym_stream;
  symbol_ S_synonym_stream_p;
  symbol_ S_synonym_stream_symbol;
  symbol_ S_make_broadcast_stream;
  symbol_ S_broadcast_stream_p;
  symbol_ S_broadcast_stream_streams;
  symbol_ S_make_concatenated_stream;
  symbol_ S_concatenated_stream_p;
  symbol_ S_concatenated_stream_streams;
  symbol_ S_make_two_way_stream;
  symbol_ S_two_way_stream_p;
  symbol_ S_two_way_stream_input_stream;
  symbol_ S_two_way_stream_output_stream;
  symbol_ S_make_echo_stream;
  symbol_ S_echo_stream_p;
  symbol_ S_echo_stream_input_stream;
  symbol_ S_echo_stream_output_stream;
  symbol_ S_make_string_input_stream;
  symbol_ S_string_input_stream_index;
  symbol_ S_make_string_output_stream;
  symbol_ S_get_output_stream_string;
  symbol_ S_make_string_push_stream;
  symbol_ S_string_stream_p;
  symbol_ S_make_buffered_input_stream;
  symbol_ S_buffered_input_stream_index;
  symbol_ S_make_buffered_output_stream;
  symbol_ S_generic_stream_controller;
  symbol_ S_make_generic_stream;
  symbol_ S_generic_stream_p;
  symbol_ S_file_stream_p;
  symbol_ S_make_keyboard_stream;
  symbol_ S_terminal_raw;
  symbol_ S_make_window;
  symbol_ S_window_size;
  symbol_ S_window_cursor_position;
  symbol_ S_set_window_cursor_position;
  symbol_ S_clear_window;
  symbol_ S_clear_window_to_eot;
  symbol_ S_clear_window_to_eol;
  symbol_ S_delete_window_line;
  symbol_ S_insert_window_line;
  symbol_ S_highlight_on;
  symbol_ S_highlight_off;
  symbol_ S_window_cursor_on;
  symbol_ S_window_cursor_off;
  symbol_ S_make_pipe_input_stream;
  symbol_ S_make_pipe_output_stream;
  symbol_ S_make_pipe_io_stream;
  symbol_ S_make_x11socket_stream;
  symbol_ S_read_n_bytes;
  symbol_ S_write_n_bytes;
  symbol_ S_socket_server;
  symbol_ S_socket_server_close;
  symbol_ S_socket_server_port;
  symbol_ S_socket_server_host;
  symbol_ S_socket_accept;
  symbol_ S_socket_wait;
  symbol_ S_socket_status;
  symbol_ S_socket_connect;
  symbol_ S_socket_stream_port;
  symbol_ S_socket_stream_host;
  symbol_ S_socket_stream_peer;
  symbol_ S_socket_stream_local;
  symbol_ S_socket_options;
  symbol_ S_socket_stream_shutdown;
  symbol_ S_make_stream;
  symbol_ S_socket_stream_handle;
  symbol_ S_built_in_stream_open_p;
  symbol_ S_input_stream_p;
  symbol_ S_output_stream_p;
  symbol_ S_stream_element_type_eq;
  symbol_ S_built_in_stream_element_type;
  symbol_ S_built_in_stream_set_element_type;
  symbol_ S_stream_external_format;
  symbol_ S_set_stream_external_format;
  symbol_ S_interactive_stream_p;
  symbol_ S_built_in_stream_close;
  symbol_ S_read_byte;
  symbol_ S_read_byte_lookahead;
  symbol_ S_read_byte_will_hang_p;
  symbol_ S_read_byte_no_hang;
  symbol_ S_read_integer;
  symbol_ S_read_float;
  symbol_ S_write_byte;
  symbol_ S_write_integer;
  symbol_ S_write_float;
  symbol_ S_file_position;
  symbol_ S_file_length;
  symbol_ S_file_string_length;
  symbol_ S_line_number;
  symbol_ S_allow_read_eval;
  symbol_ S_defgray;
  symbol_ S_putd;
  symbol_ S_find_subr;
  symbol_ S_proclaim_constant;
  symbol_ S_get;
  symbol_ S_getf;
  symbol_ S_putf;
  symbol_ S_remf;
  symbol_ S_get_properties;
  symbol_ S_putplist;
  symbol_ S_put;
  symbol_ S_remprop;
  symbol_ S_symbol_package;
  symbol_ S_symbol_plist;
  symbol_ S_symbol_name;
  symbol_ S_keywordp;
  symbol_ S_special_variable_p;
  symbol_ S_gensym;
  symbol_ S_plist;
  symbol_ S_decimal_string;
  symbol_ S_zerop;
  symbol_ S_plusp;
  symbol_ S_minusp;
  symbol_ S_oddp;
  symbol_ S_evenp;
  symbol_ S_gleich;
  symbol_ S_ungleich;
  symbol_ S_kleiner;
  symbol_ S_groesser;
  symbol_ S_klgleich;
  symbol_ S_grgleich;
  symbol_ S_max;
  symbol_ S_min;
  symbol_ S_plus;
  symbol_ S_minus;
  symbol_ S_mal;
  symbol_ S_durch;
  symbol_ S_einsplus;
  symbol_ S_einsminus;
  symbol_ S_conjugate;
  symbol_ S_gcd;
  symbol_ S_xgcd;
  symbol_ S_lcm;
  symbol_ S_exp;
  symbol_ S_expt;
  symbol_ S_log;
  symbol_ S_sqrt;
  symbol_ S_isqrt;
  symbol_ S_abs;
  symbol_ S_phase;
  symbol_ S_signum;
  symbol_ S_sin;
  symbol_ S_cos;
  symbol_ S_tan;
  symbol_ S_cis;
  symbol_ S_asin;
  symbol_ S_acos;
  symbol_ S_atan;
  symbol_ S_sinh;
  symbol_ S_cosh;
  symbol_ S_tanh;
  symbol_ S_asinh;
  symbol_ S_acosh;
  symbol_ S_atanh;
  symbol_ S_float;
  symbol_ S_rational;
  symbol_ S_rationalize;
  symbol_ S_numerator;
  symbol_ S_denominator;
  symbol_ S_floor;
  symbol_ S_ceiling;
  symbol_ S_truncate;
  symbol_ S_round;
  symbol_ S_mod;
  symbol_ S_rem;
  symbol_ S_ffloor;
  symbol_ S_fceiling;
  symbol_ S_ftruncate;
  symbol_ S_fround;
  symbol_ S_decode_float;
  symbol_ S_scale_float;
  symbol_ S_float_radix;
  symbol_ S_float_sign;
  symbol_ S_float_digits;
  symbol_ S_float_precision;
  symbol_ S_integer_decode_float;
  symbol_ S_complex;
  symbol_ S_realpart;
  symbol_ S_imagpart;
  symbol_ S_logior;
  symbol_ S_logxor;
  symbol_ S_logand;
  symbol_ S_logeqv;
  symbol_ S_lognand;
  symbol_ S_lognor;
  symbol_ S_logandc1;
  symbol_ S_logandc2;
  symbol_ S_logorc1;
  symbol_ S_logorc2;
  symbol_ S_boole;
  symbol_ S_lognot;
  symbol_ S_logtest;
  symbol_ S_logbitp;
  symbol_ S_ash;
  symbol_ S_logcount;
  symbol_ S_integer_length;
  symbol_ S_byte;
  symbol_ S_bytesize;
  symbol_ S_byteposition;
  symbol_ S_ldb;
  symbol_ S_ldb_test;
  symbol_ S_mask_field;
  symbol_ S_dpb;
  symbol_ S_deposit_field;
  symbol_ S_random;
  symbol_ S_make_random_state;
  symbol_ S_fakultaet;
  symbol_ S_exquo;
  symbol_ S_mod_expt;
  symbol_ S_long_float_digits;
  symbol_ S_set_long_float_digits;
  symbol_ S_log2;
  symbol_ S_log10;
  symbol_ S_Kallow_other_keys;
  symbol_ S_Kadjustable;
  symbol_ S_Kelement_type;
  symbol_ S_Kinitial_element;
  symbol_ S_Kinitial_contents;
  symbol_ S_Kfill_pointer;
  symbol_ S_Kdisplaced_to;
  symbol_ S_Kdisplaced_index_offset;
  symbol_ S_Kstart1;
  symbol_ S_Kend1;
  symbol_ S_Kstart2;
  symbol_ S_Kend2;
  symbol_ S_Kstart;
  symbol_ S_Kend;
  symbol_ S_Kno_hang;
  symbol_ S_Kpreserve_whitespace;
  symbol_ S_Kradix;
  symbol_ S_Kjunk_allowed;
  symbol_ S_Kcase;
  symbol_ S_Klevel;
  symbol_ S_Klength;
  symbol_ S_Klines;
  symbol_ S_Kmiser_width;
  symbol_ S_Kpprint_dispatch;
  symbol_ S_Klinear;
  symbol_ S_Kfill;
  symbol_ S_Kmiser;
  symbol_ S_Kmandatory;
  symbol_ S_Kblock;
  symbol_ S_Kcurrent;
  symbol_ S_Kgensym;
  symbol_ S_Kescape;
  symbol_ S_Kbase;
  symbol_ S_Karray;
  symbol_ S_Kcircle;
  symbol_ S_Kpretty;
  symbol_ S_Kclosure;
  symbol_ S_Kreadably;
  symbol_ S_Kright_margin;
  symbol_ S_Kstream;
  symbol_ S_Kidentity;
  symbol_ S_Ktest;
  symbol_ S_Ktest_not;
  symbol_ S_Kkey;
  symbol_ S_Knicknames;
  symbol_ S_Kuse;
  symbol_ S_Kcase_sensitive;
  symbol_ S_Kupdate;
  symbol_ S_Kup;
  symbol_ S_Kback;
  symbol_ S_Kfrom_end;
  symbol_ S_Kinitial_value;
  symbol_ S_Kcount;
  symbol_ S_Ksize;
  symbol_ S_Krehash_size;
  symbol_ S_Krehash_threshold;
  symbol_ S_Kweak;
  symbol_ S_Kboth;
  symbol_ S_Keither;
  symbol_ S_Kvalue;
  symbol_ S_Kdefaults;
  symbol_ S_Kdevice;
  symbol_ S_Kdirectory;
  symbol_ S_Kname;
  symbol_ S_Ktype;
  symbol_ S_Kversion;
  symbol_ S_Khost;
  symbol_ S_Kall;
  symbol_ S_Kmerge;
  symbol_ S_Kdirection;
  symbol_ S_Kif_exists;
  symbol_ S_Kif_does_not_exist;
  symbol_ S_Kkeep;
  symbol_ S_Kdiscard;
  symbol_ S_Kexternal_format;
  symbol_ S_Kbuffered;
  symbol_ S_Kfull;
  symbol_ S_Kabort;
  symbol_ S_Kverbose;
  symbol_ S_Kexecute;
  symbol_ S_Kcompile_toplevel;
  symbol_ S_Kload_toplevel;
  symbol_ S_Keof;
  symbol_ S_Kinput_available;
  symbol_ S_Kline_position;
  symbol_ S_Klittle;
  symbol_ S_Kbig;
  symbol_ S_Kcharset;
  symbol_ S_Kline_terminator;
  symbol_ S_Kunix;
  symbol_ S_Kmac;
  symbol_ S_Kdos;
  symbol_ S_Kinput_error_action;
  symbol_ S_Koutput_error_action;
  symbol_ S_Kansi_cl;
  symbol_ S_Kextra_file_types;
  symbol_ S_Kwait;
  symbol_ S_Kterminal;
  symbol_ S_Kpipe;
  symbol_ S_Karguments;
  symbol_ S_Kpriority;
  symbol_ S_Khigh;
  symbol_ S_Knormal;
  symbol_ S_Klow;
  symbol_ S_Ktimeout;
  symbol_ S_Kso_keepalive;
  symbol_ S_Kso_error;
  symbol_ S_Kso_linger;
  symbol_ S_Kso_oobinline;
  symbol_ S_Kso_type;
  symbol_ S_Kso_rcvbuf;
  symbol_ S_Kso_sndbuf;
  symbol_ S_Kso_rcvlowat;
  symbol_ S_Kso_sndlowat;
  symbol_ S_Kso_rcvtimeo;
  symbol_ S_Kso_sndtimeo;
  symbol_ S_Kread_only;
  symbol_ S_string_char;
  symbol_ S_base_char;
  symbol_ S_array_rank_limit;
  symbol_ S_array_dimension_limit;
  symbol_ S_array_total_size_limit;
  symbol_ S_subtype_integer;
  symbol_ S_char_cod_limit;
  symbol_ S_base_char_cod_limit;
  symbol_ S_designator;
  symbol_ S_class_slots;
  symbol_ S_slotdef_location;
  symbol_ S_slotdef_name;
  symbol_ S_structure_object;
  symbol_ S_class;
  symbol_ S_slot_missing;
  symbol_ S_slot_unbound;
  symbol_ S_reinitialize_instance_table;
  symbol_ S_make_instance_table;
  symbol_ S_initial_reinitialize_instance;
  symbol_ S_initial_initialize_instance;
  symbol_ S_initial_make_instance;
  symbol_ S_allocate_instance;
  symbol_ S_simple_vector;
  symbol_ S_simple_string;
  symbol_ S_base_string;
  symbol_ S_simple_base_string;
  symbol_ S_bit_vector;
  symbol_ S_simple_bit_vector;
  symbol_ S_array;
  symbol_ S_simple_array;
  symbol_ S_sequence;
  symbol_ S_package_error;
  symbol_ S_Kinternal;
  symbol_ S_Kexternal;
  symbol_ S_Kinherited;
  symbol_ S_do_symbols;
  symbol_ S_do_external_symbols;
  symbol_ S_packagestern;
  symbol_ S_internal_time_units_per_second;
  symbol_ S_encode_universal_time;
  symbol_ S_use_clcs;
  symbol_ S_recursive_error_count;
  symbol_ S_error_handler;
  symbol_ S_simple_condition;
  symbol_ S_simple_serious_condition;
  symbol_ S_simple_error;
  symbol_ S_simple_program_error;
  symbol_ S_simple_source_program_error;
  symbol_ S_simple_control_error;
  symbol_ S_simple_arithmetic_error;
  symbol_ S_simple_division_by_zero;
  symbol_ S_simple_floating_point_overflow;
  symbol_ S_simple_floating_point_underflow;
  symbol_ S_simple_cell_error;
  symbol_ S_simple_unbound_variable;
  symbol_ S_simple_undefined_function;
  symbol_ S_simple_unbound_slot;
  symbol_ S_simple_type_error;
  symbol_ S_simple_keyword_error;
  symbol_ S_simple_charset_type_error;
  symbol_ S_simple_package_error;
  symbol_ S_simple_print_not_readable;
  symbol_ S_simple_parse_error;
  symbol_ S_simple_stream_error;
  symbol_ S_simple_end_of_file;
  symbol_ S_simple_reader_error;
  symbol_ S_simple_file_error;
  symbol_ S_simple_os_error;
  symbol_ S_simple_storage_condition;
  symbol_ S_simple_interrupt_condition;
  symbol_ S_simple_warning;
  symbol_ S_Kinstance;
  symbol_ S_Kdatum;
  symbol_ S_Kexpected_type;
  symbol_ S_Kpackage;
  symbol_ S_Kobject;
  symbol_ S_Kpathname;
  symbol_ S_format;
  symbol_ S_debugger_hook;
  symbol_ S_coerce_to_condition;
  symbol_ S_cerror;
  symbol_ S_check_value;
  symbol_ S_break_on_signals;
  symbol_ S_safe_typep;
  symbol_ S_stream_read_byte;
  symbol_ S_stream_read_byte_lookahead;
  symbol_ S_stream_read_byte_sequence;
  symbol_ S_stream_write_byte;
  symbol_ S_stream_write_byte_sequence;
  symbol_ S_stream_read_char;
  symbol_ S_stream_unread_char;
  symbol_ S_stream_peek_char;
  symbol_ S_stream_read_char_sequence;
  symbol_ S_stream_write_char;
  symbol_ S_stream_write_char_sequence;
  symbol_ S_stream_read_line;
  symbol_ S_stream_read_char_will_hang_p;
  symbol_ S_stream_clear_input;
  symbol_ S_stream_finish_output;
  symbol_ S_stream_force_output;
  symbol_ S_stream_clear_output;
  symbol_ S_stream_line_column;
  symbol_ S_stream_position;
  symbol_ S_generic_stream_rdch;
  symbol_ S_generic_stream_pkch;
  symbol_ S_generic_stream_read_char_will_hang_p;
  symbol_ S_generic_stream_clear_input;
  symbol_ S_generic_stream_wrch;
  symbol_ S_generic_stream_wrss;
  symbol_ S_generic_stream_finish_output;
  symbol_ S_generic_stream_force_output;
  symbol_ S_generic_stream_clear_output;
  symbol_ S_generic_stream_rdby;
  symbol_ S_generic_stream_wrby;
  symbol_ S_generic_stream_close;
  symbol_ S_Kchar;
  symbol_ S_Kbits;
  symbol_ S_make_input_character;
  symbol_ S_make_char;
  symbol_ S_keyboard_input;
  symbol_ S_completion;
  symbol_ S_terminal_io;
  symbol_ S_key_bindings;
  symbol_ S_query_io;
  symbol_ S_debug_io;
  symbol_ S_standard_input;
  symbol_ S_standard_output;
  symbol_ S_error_output;
  symbol_ S_trace_output;
  symbol_ S_stream_element_type;
  symbol_ S_reval;
  symbol_ S_default_pathname_defaults;
  symbol_ S_merge_pathnames_ansi;
  symbol_ S_print_pathnames_ansi;
  symbol_ S_parse_namestring_ansi;
  symbol_ S_parse_namestring_dot_file;
  symbol_ S_logpathname_translations;
  symbol_ S_Kwild;
  symbol_ S_Kwild_inferiors;
  symbol_ S_Krelative;
  symbol_ S_Kabsolute;
  symbol_ S_Knewest;
  symbol_ S_Kunspecific;
  symbol_ S_Kcommon;
  symbol_ S_Kinput;
  symbol_ S_Kinput_immutable;
  symbol_ S_Koutput;
  symbol_ S_Kio;
  symbol_ S_Kprobe;
  symbol_ S_unsigned_byte;
  symbol_ S_signed_byte;
  symbol_ S_Kdefault;
  symbol_ S_canonicalize_type;
  symbol_ S_subtypep;
  symbol_ S_Kerror;
  symbol_ S_Knew_version;
  symbol_ S_Krename;
  symbol_ S_Krename_and_delete;
  symbol_ S_Koverwrite;
  symbol_ S_Kappend;
  symbol_ S_Ksupersede;
  symbol_ S_Kcreate;
  symbol_ S_warn;
  symbol_ S_Kignore;
  symbol_ S_with_output_to_string;
  symbol_ S_integer;
  symbol_ S_hash_table;
  symbol_ S_random_state;
  symbol_ S_reader_error;
  symbol_ S_read_base;
  symbol_ S_read_suppress;
  symbol_ S_read_eval;
  symbol_ S_readtablestern;
  symbol_ S_features;
  symbol_ S_read_preserve_whitespace;
  symbol_ S_read_line_number;
  symbol_ S_read_recursive_p;
  symbol_ S_read_reference_table;
  symbol_ S_backquote_level;
  symbol_ S_backquote_reader;
  symbol_ S_comma_reader;
  symbol_ S_reading_array;
  symbol_ S_reading_struct;
  symbol_ S_compiling;
  symbol_ S_make_init_form;
  symbol_ S_make_byte;
  symbol_ S_Kupcase;
  symbol_ S_Kdowncase;
  symbol_ S_Kcapitalize;
  symbol_ S_print_case;
  symbol_ S_print_level;
  symbol_ S_print_length;
  symbol_ S_print_gensym;
  symbol_ S_print_escape;
  symbol_ S_print_radix;
  symbol_ S_print_base;
  symbol_ S_print_array;
  symbol_ S_print_circle;
  symbol_ S_print_pretty;
  symbol_ S_print_closure;
  symbol_ S_print_readably;
  symbol_ S_print_lines;
  symbol_ S_print_miser_width;
  symbol_ S_print_pprint_dispatch;
  symbol_ S_print_right_margin;
  symbol_ S_print_rpars;
  symbol_ S_print_indent_lists;
  symbol_ S_print_pretty_fill;
  symbol_ S_print_circle_table;
  symbol_ S_prin_level;
  symbol_ S_prin_lines;
  symbol_ S_prin_line_prefix;
  symbol_ S_prin_miserp;
  symbol_ S_prin_pprinter;
  symbol_ S_prin_indentation;
  symbol_ S_prin_bqlevel;
  symbol_ S_prin_stream;
  symbol_ S_prin_linelength;
  symbol_ S_prin_l1;
  symbol_ S_prin_lm;
  symbol_ S_prin_rpar;
  symbol_ S_prin_traillength;
  symbol_ S_prin_prev_traillength;
  symbol_ S_prin_jblocks;
  symbol_ S_prin_jbstrings;
  symbol_ S_prin_jbmodus;
  symbol_ S_prin_jblpos;
  symbol_ S_format_tabulate;
  symbol_ S_load_forms;
  symbol_ S_terminal_read_open_object;
  symbol_ S_terminal_read_stream;
  symbol_ S_backquote;
  symbol_ S_splice;
  symbol_ S_nsplice;
  symbol_ S_unquote;
  symbol_ S_structure_print;
  symbol_ S_defstruct_description;
  symbol_ S_print_object;
  symbol_ S_trace_values;
  symbol_ S_setf_function;
  symbol_ S_lambda;
  symbol_ S_LLoptional;
  symbol_ S_LLkey;
  symbol_ S_LLallow_other_keys;
  symbol_ S_LLrest;
  symbol_ S_LLaux;
  symbol_ S_LLbody;
  symbol_ S_macro;
  symbol_ S_special;
  symbol_ S_source;
  symbol_ S_optimize;
  symbol_ S_declaration;
  symbol_ S_compile_lambda;
  symbol_ S_expand_lambdabody_main;
  symbol_ S_compile;
  symbol_ S_evalhookstern;
  symbol_ S_applyhookstern;
  symbol_ S_macroexpand_hook;
  symbol_ S_lambda_parameters_limit;
  symbol_ S_call_arguments_limit;
  symbol_ S_multiple_values_limit;
  symbol_ S_jmpbuf_size;
  symbol_ S_big_endian;
  symbol_ S_Klambda;
  symbol_ S_keyword;
  symbol_ S_plus2;
  symbol_ S_plus3;
  symbol_ S_mal2;
  symbol_ S_mal3;
  symbol_ S_durch2;
  symbol_ S_durch3;
  symbol_ S_driverstern;
  symbol_ S_break_driver;
  symbol_ S_break_count;
  symbol_ S_recurse_count_standard_output;
  symbol_ S_recurse_count_debug_io;
  symbol_ S_frame_limit1;
  symbol_ S_frame_limit2;
  symbol_ S_setf;
  symbol_ S_psetf;
  symbol_ S_multiple_value_setf;
  symbol_ S_make_macro_expander;
  symbol_ S_type_for_discrimination;
  symbol_ S_pthe;
  symbol_ S_compile_form;
  symbol_ S_otherwise;
  symbol_ S_inline;
  symbol_ S_notinline;
  symbol_ S_get_funname_symbol;
  symbol_ S_inlinable;
  symbol_ S_constant_inline;
  symbol_ S_constant_notinline;
  symbol_ S_constant_inlinable;
  symbol_ S_boolean;
  symbol_ S_symbol;
  symbol_ S_address;
  symbol_ S_file_stream;
  symbol_ S_synonym_stream;
  symbol_ S_broadcast_stream;
  symbol_ S_concatenated_stream;
  symbol_ S_two_way_stream;
  symbol_ S_echo_stream;
  symbol_ S_string_stream;
  symbol_ S_stream;
  symbol_ S_package;
  symbol_ S_readtable;
  symbol_ S_special_operator;
  symbol_ S_load_time_eval;
  symbol_ S_symbol_macro;
  symbol_ S_function_macro;
  symbol_ S_encoding;
  symbol_ S_foreign_pointer;
  symbol_ S_weak_pointer;
  symbol_ S_weak_kvtable;
  symbol_ S_finalizer;
  symbol_ S_compiled_function;
  symbol_ S_frame_pointer;
  symbol_ S_read_label;
  symbol_ S_system_internal;
  symbol_ S_fixnum;
  symbol_ S_bignum;
  symbol_ S_ratio;
  symbol_ S_short_float;
  symbol_ S_single_float;
  symbol_ S_double_float;
  symbol_ S_long_float;
  symbol_ S_standard_generic_function;
  symbol_ S_closclass;
  symbol_ S_typep;
  symbol_ S_deftype_expander;
  symbol_ S_expand_deftype;
  symbol_ S_deftype_depth_limit;
  symbol_ S_gc_statistics_stern;
  symbol_ S_recurse_count_gc_statistics;
  symbol_ S_traced_definition;
  symbol_ S_gensym_counter;
  symbol_ S_pprint_first_newline;
  symbol_ S_print_symbols_long;
  symbol_ S_inhibit_floating_point_underflow;
  symbol_ S_warn_on_floating_point_contagion;
  symbol_ S_floating_point_contagion_ansi;
  symbol_ S_pi;
  symbol_ S_number;
  symbol_ S_real;
  symbol_ S_most_positive_fixnum;
  symbol_ S_most_negative_fixnum;
  symbol_ S_most_positive_short_float;
  symbol_ S_least_positive_short_float;
  symbol_ S_least_negative_short_float;
  symbol_ S_most_negative_short_float;
  symbol_ S_most_positive_single_float;
  symbol_ S_least_positive_single_float;
  symbol_ S_least_negative_single_float;
  symbol_ S_most_negative_single_float;
  symbol_ S_most_positive_double_float;
  symbol_ S_least_positive_double_float;
  symbol_ S_least_negative_double_float;
  symbol_ S_most_negative_double_float;
  symbol_ S_most_positive_long_float;
  symbol_ S_least_positive_long_float;
  symbol_ S_least_negative_long_float;
  symbol_ S_most_negative_long_float;
  symbol_ S_least_positive_normalized_long_float;
  symbol_ S_least_negative_normalized_long_float;
  symbol_ S_short_float_epsilon;
  symbol_ S_single_float_epsilon;
  symbol_ S_double_float_epsilon;
  symbol_ S_long_float_epsilon;
  symbol_ S_short_float_negative_epsilon;
  symbol_ S_single_float_negative_epsilon;
  symbol_ S_double_float_negative_epsilon;
  symbol_ S_long_float_negative_epsilon;
  symbol_ S_default_float_format;
  symbol_ S_read_default_float_format;
  symbol_ S_write_float_decimal;
  symbol_ S_random_state_stern;
  symbol_ S_unicode_16;
  symbol_ S_unicode_16_big_endian;
  symbol_ S_unicode_16_little_endian;
  symbol_ S_unicode_32;
  symbol_ S_unicode_32_big_endian;
  symbol_ S_unicode_32_little_endian;
  symbol_ S_utf_8;
  symbol_ S_java;
  symbol_ S_ascii;
  symbol_ S_iso8859_1;
  symbol_ S_iso8859_2;
  symbol_ S_iso8859_3;
  symbol_ S_iso8859_4;
  symbol_ S_iso8859_5;
  symbol_ S_iso8859_6;
  symbol_ S_iso8859_7;
  symbol_ S_iso8859_8;
  symbol_ S_iso8859_9;
  symbol_ S_iso8859_10;
  symbol_ S_iso8859_13;
  symbol_ S_iso8859_14;
  symbol_ S_iso8859_15;
  symbol_ S_iso8859_16;
  symbol_ S_koi8_r;
  symbol_ S_koi8_u;
  symbol_ S_mac_arabic;
  symbol_ S_mac_centraleurope;
  symbol_ S_mac_croatian;
  symbol_ S_mac_cyrillic;
  symbol_ S_mac_dingbat;
  symbol_ S_mac_greek;
  symbol_ S_mac_hebrew;
  symbol_ S_mac_iceland;
  symbol_ S_mac_roman;
  symbol_ S_mac_romania;
  symbol_ S_mac_symbol;
  symbol_ S_mac_thai;
  symbol_ S_mac_turkish;
  symbol_ S_mac_ukraine;
  symbol_ S_cp437_ms;
  symbol_ S_cp437_ibm;
  symbol_ S_cp737;
  symbol_ S_cp775;
  symbol_ S_cp850;
  symbol_ S_cp852_ms;
  symbol_ S_cp852_ibm;
  symbol_ S_cp855;
  symbol_ S_cp857;
  symbol_ S_cp860_ms;
  symbol_ S_cp860_ibm;
  symbol_ S_cp861_ms;
  symbol_ S_cp861_ibm;
  symbol_ S_cp862_ms;
  symbol_ S_cp862_ibm;
  symbol_ S_cp863_ms;
  symbol_ S_cp863_ibm;
  symbol_ S_cp864_ms;
  symbol_ S_cp864_ibm;
  symbol_ S_cp865_ms;
  symbol_ S_cp865_ibm;
  symbol_ S_cp866;
  symbol_ S_cp869_ms;
  symbol_ S_cp869_ibm;
  symbol_ S_cp874_ms;
  symbol_ S_cp874_ibm;
  symbol_ S_cp1250;
  symbol_ S_cp1251;
  symbol_ S_cp1252;
  symbol_ S_cp1253;
  symbol_ S_cp1254;
  symbol_ S_cp1256;
  symbol_ S_cp1257;
  symbol_ S_hp_roman8;
  symbol_ S_nextstep;
  symbol_ S_jisx0201;
  symbol_ S_ucs_2;
  symbol_ S_ucs_4;
  symbol_ S_macintosh;
  symbol_ S_windows_1250;
  symbol_ S_windows_1251;
  symbol_ S_windows_1252;
  symbol_ S_windows_1253;
  symbol_ S_windows_1254;
  symbol_ S_windows_1256;
  symbol_ S_windows_1257;
  symbol_ S_koi8_ru;
  symbol_ S_cp1255;
  symbol_ S_cp1258;
  symbol_ S_euc_jp;
  symbol_ S_shift_jis;
  symbol_ S_cp932;
  symbol_ S_iso_2022_jp;
  symbol_ S_iso_2022_jp_2;
  symbol_ S_iso_2022_jp_1;
  symbol_ S_euc_cn;
  symbol_ S_hz;
  symbol_ S_gbk;
  symbol_ S_cp936;
  symbol_ S_gb18030;
  symbol_ S_euc_tw;
  symbol_ S_big5;
  symbol_ S_cp950;
  symbol_ S_big5_hkscs;
  symbol_ S_iso_2022_cn;
  symbol_ S_iso_2022_cn_ext;
  symbol_ S_euc_kr;
  symbol_ S_cp949;
  symbol_ S_iso_2022_kr;
  symbol_ S_johab;
  symbol_ S_armscii_8;
  symbol_ S_georgian_academy;
  symbol_ S_georgian_ps;
  symbol_ S_tis_620;
  symbol_ S_mulelao_1;
  symbol_ S_cp1133;
  symbol_ S_viscii;
  symbol_ S_tcvn;
  symbol_ S_utf_16;
  symbol_ S_utf_7;
  symbol_ S_windows_1255;
  symbol_ S_windows_1258;
  symbol_ S_english;
  symbol_ S_init_hooks;
  symbol_ S_quiet;
  symbol_ S_Klisting;
  symbol_ S_Koutput_file;
  symbol_ S_compile_file;
  symbol_ S_load_compiling;
  symbol_ S_load_verbose;
  symbol_ S_load_print;
  symbol_ S_compile_print;
  symbol_ S_compile_verbose;
  symbol_ S_args;
  symbol_ S_appease_cerrors;
  symbol_ S_batchmode_errors;
  symbol_ S_wait_keypress;
} symbol_tab_data;
#define S(name)  S_help_(S_##name)
#define symbol_tab  symbol_tab_data
#define S_help_(name)  objectplus(&symbol_tab.name,1UL)
#define NIL  S(nil)
#define T    S(t)
extern struct object_tab_ {
  gcv_object_t all_weakpointers;
  gcv_object_t all_weakkvtables;
  gcv_object_t all_finalizers;
  gcv_object_t pending_finalizers;
  gcv_object_t default_file_encoding;
  gcv_object_t internal_encoding;
  gcv_object_t pathname_encoding;
  gcv_object_t terminal_encoding;
  gcv_object_t misc_encoding;
  gcv_object_t type_line_terminator;
  gcv_object_t type_input_error_action;
  gcv_object_t type_output_error_action;
  gcv_object_t charname_0bis;
  gcv_object_t charname_7bis;
  gcv_object_t charname_8bis;
  gcv_object_t charname_9bis;
  gcv_object_t charname_10bis;
  gcv_object_t charname_10tris;
  gcv_object_t charname_12bis;
  gcv_object_t charname_13bis;
  gcv_object_t charname_27bis;
  gcv_object_t charname_32bis;
  gcv_object_t charname_127bis;
  gcv_object_t charname_127tris;
  gcv_object_t charname_0;
  gcv_object_t charname_1;
  gcv_object_t charname_2;
  gcv_object_t charname_3;
  gcv_object_t charname_4;
  gcv_object_t charname_5;
  gcv_object_t charname_6;
  gcv_object_t charname_7;
  gcv_object_t charname_8;
  gcv_object_t charname_9;
  gcv_object_t charname_10;
  gcv_object_t charname_11;
  gcv_object_t charname_12;
  gcv_object_t charname_13;
  gcv_object_t charname_14;
  gcv_object_t charname_15;
  gcv_object_t charname_16;
  gcv_object_t charname_17;
  gcv_object_t charname_18;
  gcv_object_t charname_19;
  gcv_object_t charname_20;
  gcv_object_t charname_21;
  gcv_object_t charname_22;
  gcv_object_t charname_23;
  gcv_object_t charname_24;
  gcv_object_t charname_25;
  gcv_object_t charname_26;
  gcv_object_t charname_27;
  gcv_object_t charname_28;
  gcv_object_t charname_29;
  gcv_object_t charname_30;
  gcv_object_t charname_31;
  gcv_object_t charname_32;
  gcv_object_t charname_127;
  gcv_object_t type_vector_with_fill_pointer;
  gcv_object_t type_weak_ht;
  gcv_object_t seq_types;
  gcv_object_t type_recognizable_sequence_type;
  gcv_object_t kwpair_start;
  gcv_object_t kwpair_end;
  gcv_object_t kwpair_start1;
  gcv_object_t kwpair_end1;
  gcv_object_t kwpair_start2;
  gcv_object_t kwpair_end2;
  gcv_object_t class_structure_types;
  gcv_object_t class_array;
  gcv_object_t class_bit_vector;
  gcv_object_t class_character;
  gcv_object_t class_complex;
  gcv_object_t class_cons;
  gcv_object_t class_float;
  gcv_object_t class_function;
  gcv_object_t class_hash_table;
  gcv_object_t class_integer;
  gcv_object_t class_null;
  gcv_object_t class_package;
  gcv_object_t class_pathname;
  gcv_object_t class_logical_pathname;
  gcv_object_t class_random_state;
  gcv_object_t class_ratio;
  gcv_object_t class_readtable;
  gcv_object_t class_standard_generic_function;
  gcv_object_t class_stream;
  gcv_object_t class_file_stream;
  gcv_object_t class_synonym_stream;
  gcv_object_t class_broadcast_stream;
  gcv_object_t class_concatenated_stream;
  gcv_object_t class_two_way_stream;
  gcv_object_t class_echo_stream;
  gcv_object_t class_string_stream;
  gcv_object_t class_string;
  gcv_object_t class_symbol;
  gcv_object_t class_t;
  gcv_object_t class_vector;
  gcv_object_t type_designator_character;
  gcv_object_t type_designator_function;
  gcv_object_t structure_class_count_max;
  gcv_object_t standard_class_count_max;
  gcv_object_t hs_t;
  gcv_object_t hs_cons;
  gcv_object_t hs_null;
  gcv_object_t hs_symbol;
  gcv_object_t hs_simple_bit_vector;
  gcv_object_t hs_simple_2bit_vector;
  gcv_object_t hs_simple_4bit_vector;
  gcv_object_t hs_simple_8bit_vector;
  gcv_object_t hs_simple_16bit_vector;
  gcv_object_t hs_simple_32bit_vector;
  gcv_object_t hs_simple_string;
  gcv_object_t hs_simple_nilvector;
  gcv_object_t hs_simple_vector;
  gcv_object_t hs_bit_vector;
  gcv_object_t hs_2bit_vector;
  gcv_object_t hs_4bit_vector;
  gcv_object_t hs_8bit_vector;
  gcv_object_t hs_16bit_vector;
  gcv_object_t hs_32bit_vector;
  gcv_object_t hs_string;
  gcv_object_t hs_nilvector;
  gcv_object_t hs_vector;
  gcv_object_t hs_simple_array;
  gcv_object_t hs_array;
  gcv_object_t hs_standard_generic_function;
  gcv_object_t hs_function;
  gcv_object_t hs_file_stream;
  gcv_object_t hs_synonym_stream;
  gcv_object_t hs_broadcast_stream;
  gcv_object_t hs_concatenated_stream;
  gcv_object_t hs_two_way_stream;
  gcv_object_t hs_echo_stream;
  gcv_object_t hs_string_stream;
  gcv_object_t hs_stream;
  gcv_object_t hs_hash_table;
  gcv_object_t hs_package;
  gcv_object_t hs_readtable;
  gcv_object_t hs_pathname;
  gcv_object_t hs_logical_pathname;
  gcv_object_t hs_random_state;
  gcv_object_t hs_byte;
  gcv_object_t hs_special_operator;
  gcv_object_t hs_load_time_eval;
  gcv_object_t hs_symbol_macro;
  gcv_object_t hs_macro;
  gcv_object_t hs_function_macro;
  gcv_object_t hs_encoding;
  gcv_object_t hs_foreign_pointer;
  gcv_object_t hs_weakpointer;
  gcv_object_t hs_weakkvt;
  gcv_object_t hs_finalizer;
  gcv_object_t hs_socket_server;
  gcv_object_t hs_system_function;
  gcv_object_t hs_bignum;
  gcv_object_t hs_ratio;
  gcv_object_t hs_single_float;
  gcv_object_t hs_double_float;
  gcv_object_t hs_long_float;
  gcv_object_t hs_complex;
  gcv_object_t gc_statistics_list;
  gcv_object_t all_packages;
  gcv_object_t keyword_package;
  gcv_object_t charset_package;
  gcv_object_t default_package;
  gcv_object_t query_string_10sp;
  gcv_object_t query_string_2dash;
  gcv_object_t query_string_prompt;
  gcv_object_t export_string_1;
  gcv_object_t export_string_2;
  gcv_object_t use_default;
  gcv_object_t ansi_user_package_name;
  gcv_object_t gensym_prefix;
  gcv_object_t lisp_implementation_type_string;
  gcv_object_t lisp_implementation_package_version;
  gcv_object_t lisp_implementation_version_built_string;
  gcv_object_t lisp_implementation_version_string;
  gcv_object_t memory_image_timestamp;
  gcv_object_t memory_image_host;
  gcv_object_t version;
  gcv_object_t machine_type_string;
  gcv_object_t machine_version_string;
  gcv_object_t machine_instance_string;
  gcv_object_t c_compiler_version;
  gcv_object_t argv;
  gcv_object_t current_language;
  gcv_object_t ansi;
  gcv_object_t error_string1;
  gcv_object_t error_types;
  gcv_object_t type_uint8;
  gcv_object_t type_sint8;
  gcv_object_t type_uint16;
  gcv_object_t type_sint16;
  gcv_object_t type_uint32;
  gcv_object_t type_sint32;
  gcv_object_t type_uint64;
  gcv_object_t type_sint64;
  gcv_object_t type_array_index;
  gcv_object_t type_array_bit;
  gcv_object_t type_posfixnum;
  gcv_object_t type_negfixnum;
  gcv_object_t type_posbignum;
  gcv_object_t type_negbignum;
  gcv_object_t type_posfixnum1;
  gcv_object_t type_array_rank;
  gcv_object_t type_radix;
  gcv_object_t type_end_index;
  gcv_object_t type_posinteger;
  gcv_object_t type_stringsymchar;
  gcv_object_t type_svector2;
  gcv_object_t type_svector5;
  gcv_object_t type_climb_mode;
  gcv_object_t type_hashtable_test;
  gcv_object_t type_hashtable_size;
  gcv_object_t type_hashtable_rehash_size;
  gcv_object_t type_hashtable_rehash_threshold;
  gcv_object_t type_boole;
  gcv_object_t type_not_digit;
  gcv_object_t type_rtcase;
  gcv_object_t type_peektype;
  gcv_object_t type_printcase;
  gcv_object_t type_pprint_newline;
  gcv_object_t type_pprint_indent;
  gcv_object_t type_random_arg;
  gcv_object_t type_packname;
  gcv_object_t type_posint16;
  gcv_object_t type_string_integer;
  gcv_object_t type_uint8_vector;
  gcv_object_t type_position;
  gcv_object_t type_host;
  gcv_object_t type_version;
  gcv_object_t type_direction;
  gcv_object_t type_if_exists;
  gcv_object_t type_if_does_not_exist;
  gcv_object_t type_directory_not_exist;
  gcv_object_t type_external_format;
  gcv_object_t type_pathname_field_key;
  gcv_object_t type_socket_option;
  gcv_object_t type_logical_pathname;
  gcv_object_t type_builtin_stream;
  gcv_object_t lib_dir;
  gcv_object_t type_designator_pathname;
  gcv_object_t type_priority;
  gcv_object_t empty_logical_pathname;
  gcv_object_t default_logical_pathname_host;
  gcv_object_t empty_string;
  gcv_object_t wild_string;
  gcv_object_t colon_string;
  gcv_object_t semicolon_string;
  gcv_object_t zero_string;
  gcv_object_t slash_string;
  gcv_object_t dot_string;
  gcv_object_t dotdot_string;
  gcv_object_t dotdotdot_string;
  gcv_object_t backupextend_string;
  gcv_object_t wildwild_string;
  gcv_object_t directory_absolute;
  gcv_object_t user_homedir;
  gcv_object_t command_shell;
  gcv_object_t command_shell_option;
  gcv_object_t user_shell;
  gcv_object_t open_files;
  gcv_object_t files_to_close;
  gcv_object_t base10_radixnil;
  gcv_object_t directory_default;
  gcv_object_t source_file_type;
  gcv_object_t compiled_file_type;
  gcv_object_t listing_file_type;
  gcv_object_t dynamic_8bit_vector;
  gcv_object_t dynamic_string;
  gcv_object_t class_fundamental_stream;
  gcv_object_t class_fundamental_input_stream;
  gcv_object_t class_fundamental_output_stream;
  gcv_object_t type_input_stream;
  gcv_object_t type_output_stream;
  gcv_object_t type_string_with_fill_pointer;
  gcv_object_t setf_stream_element_type;
  gcv_object_t type_endianness;
  gcv_object_t type_open_file_stream;
  gcv_object_t strmtype_ubyte8;
  gcv_object_t rtcase_0;
  gcv_object_t rtcase_1;
  gcv_object_t rtcase_2;
  gcv_object_t rtcase_3;
  gcv_object_t standard_readtable;
  gcv_object_t dispatch_reader;
  gcv_object_t dispatch_reader_index;
  gcv_object_t charname_prefix;
  gcv_object_t token_buff_1;
  gcv_object_t token_buff_2;
  gcv_object_t displaced_string;
  gcv_object_t handler_for_arithmetic_error;
  gcv_object_t tildeA;
  gcv_object_t printstring_array;
  gcv_object_t printstring_fill_pointer;
  gcv_object_t printstring_address;
  gcv_object_t printstring_system;
  gcv_object_t printstring_frame_pointer;
  gcv_object_t printstring_read_label;
  gcv_object_t printstring_unbound;
  gcv_object_t printstring_special_reference;
  gcv_object_t printstring_disabled_pointer;
  gcv_object_t printstring_dot;
  gcv_object_t printstring_eof;
  gcv_object_t printstring_deleted;
  gcv_object_t printstring_package;
  gcv_object_t printstring_readtable;
  gcv_object_t pathname_slotlist;
  gcv_object_t byte_slotlist;
  gcv_object_t printstring_symbolmacro;
  gcv_object_t printstring_macro;
  gcv_object_t printstring_functionmacro;
  gcv_object_t printstring_encoding;
  gcv_object_t printstring_invalid;
  gcv_object_t printstring_fpointer;
  gcv_object_t printstring_weakpointer;
  gcv_object_t printstring_broken_weakpointer;
  gcv_object_t printstring_finalizer;
  gcv_object_t printstring_socket_server;
  gcv_object_t printstring_closure;
  gcv_object_t printstring_generic_function;
  gcv_object_t printstring_compiled_closure;
  gcv_object_t printstring_subr;
  gcv_object_t printstring_addon_subr;
  gcv_object_t printstring_fsubr;
  gcv_object_t printstring_closed;
  gcv_object_t printstring_input;
  gcv_object_t printstring_output;
  gcv_object_t printstring_io;
  gcv_object_t printstring_buffered;
  gcv_object_t printstring_unbuffered;
  gcv_object_t printstring_strmtype_synonym;
  gcv_object_t printstring_strmtype_broad;
  gcv_object_t printstring_strmtype_concat;
  gcv_object_t printstring_strmtype_twoway;
  gcv_object_t printstring_strmtype_echo;
  gcv_object_t printstring_strmtype_str_in;
  gcv_object_t printstring_strmtype_str_out;
  gcv_object_t printstring_strmtype_str_push;
  gcv_object_t printstring_strmtype_pphelp;
  gcv_object_t printstring_strmtype_buff_in;
  gcv_object_t printstring_strmtype_buff_out;
  gcv_object_t printstring_strmtype_generic;
  gcv_object_t printstring_strmtype_file;
  gcv_object_t printstring_strmtype_keyboard;
  gcv_object_t printstring_strmtype_terminal;
  gcv_object_t printstring_strmtype_window;
  gcv_object_t printstring_strmtype_pipe_in;
  gcv_object_t printstring_strmtype_pipe_out;
  gcv_object_t printstring_strmtype_x11socket;
  gcv_object_t printstring_strmtype_socket;
  gcv_object_t printstring_strmtype_twoway_socket;
  gcv_object_t printstring_stream;
  gcv_object_t FF_zero;
  gcv_object_t FF_one;
  gcv_object_t FF_minusone;
  gcv_object_t DF_zero;
  gcv_object_t DF_one;
  gcv_object_t DF_minusone;
  gcv_object_t LF_digits;
  gcv_object_t SF_pi;
  gcv_object_t FF_pi;
  gcv_object_t DF_pi;
  gcv_object_t pi;
  gcv_object_t LF_pi;
  gcv_object_t LF_ln2;
  gcv_object_t LF_ln10;
  gcv_object_t top_decl_env;
  gcv_object_t declaration_types;
  gcv_object_t common_lisp_string;
  gcv_object_t newline_string;
  gcv_object_t prompt_string;
  gcv_object_t breakprompt_string;
  gcv_object_t showstack_string_lisp_obj;
  gcv_object_t showstack_string_bindung;
  gcv_object_t showstack_string_zuord;
  gcv_object_t showstack_string_zuordtag;
  gcv_object_t showstack_string_VENV_frame;
  gcv_object_t showstack_string_FENV_frame;
  gcv_object_t showstack_string_BENV_frame;
  gcv_object_t showstack_string_GENV_frame;
  gcv_object_t showstack_string_DENV_frame;
  gcv_object_t seclass_no_se;
  gcv_object_t seclass_read;
  gcv_object_t seclass_write;
  gcv_object_t seclass_default;
} object_tab;
#define GLO(name)  (object_tab.name)
extern uintC module_count;
typedef struct { const char* packname; const char* symname; } subr_initdata_t;
typedef struct { const char* initstring; } object_initdata_t;
typedef struct module_t { const char* name; subr_t* stab; const uintC* stab_size; gcv_object_t* otab; const uintC* otab_size; bool initialized; const subr_initdata_t* stab_initdata; const object_initdata_t* otab_initdata; void (*initfunction1) (struct module_t *); void (*initfunction2) (struct module_t *); } module_t;
extern module_t modules[];
#define STACK_(n)  (STACK[-1-(sintP)(n)])
#define skipSTACKop  -=
#define STACKop      -
#define STACK_0  (STACK_(0))
#define STACK_1  (STACK_(1))
#define STACK_2  (STACK_(2))
#define STACK_3  (STACK_(3))
#define STACK_4  (STACK_(4))
#define STACK_5  (STACK_(5))
#define STACK_6  (STACK_(6))
#define STACK_7  (STACK_(7))
#define STACK_8  (STACK_(8))
#define STACK_9  (STACK_(9))
#define STACK_10  (STACK_(10))
#define pushSTACK(obj)  (STACK_(-1) = (obj), STACK skipSTACKop -1)
#define popSTACK()  (STACK skipSTACKop 1, STACK_(-1))
#define skipSTACK(n)  (STACK skipSTACKop (sintP)(n))
extern uintC mv_count;
extern object mv_space [127];
#define value1  mv_space[0]
struct backtrace_t {
  struct backtrace_t* bt_next;
  gcv_object_t bt_caller;
  gcv_object_t *bt_stack;
  int bt_num_arg;
};
typedef struct backtrace_t * p_backtrace_t;
#define subr_self  back_trace->bt_caller
extern p_backtrace_t back_trace;
#define value2  mv_space[1]
#define value3  mv_space[2]
#define value4  mv_space[3]
#define value5  mv_space[4]
#define value6  mv_space[5]
#define value7  mv_space[6]
#define value8  mv_space[7]
#define value9  mv_space[8]
#define VALUES0 do{ value1 = NIL; mv_count = 0; }while(0)
#define VALUES1(A) do{ value1 = (A); mv_count = 1; }while(0)
#define VALUES2(A,B) do{ value1 = (A); value2 = (B); mv_count = 2;}while(0)
#define VALUES3(A,B,C) do{ value1 = (A); value2 = (B); value3 = (C); mv_count = 3;}while(0)
#define VALUES_IF(C) do{ value1 = (C) ? T : NIL; mv_count = 1; }while(0)
#define args_end_pointer  STACK
#define makebottomword(type,size)  as_object((oint)(type)+(oint)(size))
#define framecode(bottomword)  (as_oint(bottomword) & minus_wbit(26UL))
#define framebottomword(type,top_of_frame,bot_of_frame)  makebottomword(type,(uintP)(bot_of_frame)-(uintP)(top_of_frame))
#define finish_frame(frametype)  (STACK_(-1) = framebottomword(frametype##_frame_info,top_of_frame,STACK STACKop -1), skipSTACK(-1))
extern Values funcall (object fun, uintC argcount);
#define LISPFUNN(name,req_anz)  LISPFUN(name,sec,req_anz,0,norest,nokey,0,NIL)
#define LISPFUN_B(name,sec,req_anz,opt_anz,rest_flag,key_flag,key_anz,keywords)  extern Values C_##name subr_##rest_flag##_function_args
#define subr_norest_function_args  (void)
#define subr_rest_function_args  (uintC argcount, object* rest_args_pointer)
#define LISPFUN_F(name,sec,req_anz,opt_anz,rest_flag,key_flag,key_anz,keywords)  { nullobj, 268566569UL, nullobj, nullobj, (lisp_function_t)(&C_##name), 0, req_anz, opt_anz, (uintB)subr_##rest_flag, (uintB)subr_##key_flag, key_anz, sec},
#define LISPFUN  LISPFUN_B
extern object n_char_to_string (const char* charptr, uintL len, object encoding);
extern object gethash (object obj, object ht);
extern object asciz_to_string (const char * asciz, object encoding);
extern object ascii_to_string (const char * asciz);
extern object string_concat (uintC argcount);
extern object string_to_asciz (object obj, object encoding);
extern object unpack_string_ro (object string, uintL* len, uintL* offset);
extern uintL cslen_f (object encoding, const chart*src, uintL srclen);
extern void cstombs_f (object encoding, const chart *src, uintL srclen, uintB* dest, uintL destlen);
#define with_string_0(string,encoding,ascizvar,statement)     do { var uintL ascizvar##_len;    var uintL ascizvar##_offset;    var object ascizvar##_string = unpack_string_ro(string,&ascizvar##_len,&ascizvar##_offset);    var const chart* ptr1;    unpack_sstring_alloca(ascizvar##_string,ascizvar##_len,ascizvar##_offset, ptr1=);   {var uintL ascizvar##_bytelen = cslen_f(encoding,ptr1,ascizvar##_len);    var DYNAMIC_ARRAY(ascizvar##_data,uintB,ascizvar##_bytelen+1);    cstombs_f(encoding,ptr1,ascizvar##_len,&ascizvar##_data[0],ascizvar##_bytelen);    ascizvar##_data[ascizvar##_bytelen] = 0;    {var char* ascizvar = (char*) &ascizvar##_data[0];     statement    }    FREE_DYNAMIC_ARRAY(ascizvar##_data);  }} while(0)
#define with_sstring_0(string,encoding,ascizvar,statement)  do { var object ascizvar##_string = (string);    simple_array_to_storage(ascizvar##_string);   {var uintL ascizvar##_len = Sstring_length(ascizvar##_string);    var const chart* ptr1;    unpack_sstring_alloca(ascizvar##_string,ascizvar##_len,0, ptr1=);   {var uintL ascizvar##_bytelen = cslen_f(encoding,ptr1,ascizvar##_len);    var DYNAMIC_ARRAY(ascizvar##_data,uintB,ascizvar##_bytelen+1);    cstombs_f(encoding,ptr1,ascizvar##_len,&ascizvar##_data[0],ascizvar##_bytelen);    ascizvar##_data[ascizvar##_bytelen] = 0;    {var char* ascizvar = (char*) &ascizvar##_data[0];     statement    }    FREE_DYNAMIC_ARRAY(ascizvar##_data);  }}} while(0)
extern void copy_8bit_16bit (const uint8* src, uint16* dest, uintL len);
extern void copy_8bit_32bit (const uint8* src, uint32* dest, uintL len);
extern void copy_16bit_8bit (const uint16* src, uint8* dest, uintL len);
extern void copy_16bit_16bit (const uint16* src, uint16* dest, uintL len);
extern void copy_16bit_32bit (const uint16* src, uint32* dest, uintL len);
extern void copy_32bit_8bit (const uint32* src, uint8* dest, uintL len);
extern void copy_32bit_16bit (const uint32* src, uint16* dest, uintL len);
#define TheS8string(obj) ((S8string)(ngci_pointable(obj)-1))
#define TheS16string(obj) ((S16string)(ngci_pointable(obj)-1))
#define TheS32string(obj) ((S32string)(ngci_pointable(obj)-1))
typedef uint8 cint8;
typedef uint16 cint16;
typedef uint32 cint32;
#define STRUCT_SSTRING(cint_type)  struct {    LRECORD_HEADER    cint_type  data[unspecified];  }
typedef STRUCT_SSTRING(cint8) s8string_;
typedef s8string_ * S8string;
typedef STRUCT_SSTRING(cint16) s16string_;
typedef s16string_ * S16string;
typedef STRUCT_SSTRING(cint32) s32string_;
typedef s32string_ * S32string;
#define unpack_sstring_alloca(string,len,offset,charptr_assignment)  if (Record_type(string) == 21      || Record_type(string) == 22) {    charptr_assignment (const chart*) &TheS32string(string)->data[offset];  } else {    var chart* _unpacked_ = (chart*)alloca((len)*sizeof(chart));    if ((len) > 0) {      if (Record_type(string) == 19          || Record_type(string) == 20)        copy_16bit_32bit(&TheS16string(string)->data[offset],(cint32*)_unpacked_,len);      else if (Record_type(string) == 17               || Record_type(string) == 18)        copy_8bit_32bit(&TheS8string(string)->data[offset],(cint32*)_unpacked_,len);      else        NOTREACHED;    }    charptr_assignment (const chart*) _unpacked_;  }
#define TheAsciz(obj)  ((char*)(&TheSbvector(obj)->data[0]))
extern object vectorof (uintC len);
extern object listof (uintC len);
extern object nreverse (object list);
extern object deleteq (object list, object obj);
extern object memq (const object obj, const object lis);
typedef enum { condition=0, serious_condition=1, error=2, program_error=3, source_program_error=4, control_error=5, arithmetic_error=6, division_by_zero=7, floating_point_overflow=8, floating_point_underflow=9, cell_error=10, unbound_variable=11, undefined_function=12, unbound_slot=13, type_error=14, keyword_error=15, charset_type_error=16, package_error=17, print_not_readable=18, parse_error=19, stream_error=20, end_of_file=21, reader_error=22, file_error=23, os_error=24, storage_condition=25, interrupt_condition=26, warning=27 } condition_t;
nonreturning_function(extern, fehler, (condition_t errortype, const char * errorstring));
nonreturning_function(extern, OS_error, (void));
nonreturning_function(extern, OS_file_error, (object pathname));
nonreturning_function(extern, OS_filestream_error, (object stream));
nonreturning_function(extern, fehler_string_integer, (object obj));
nonreturning_function(extern, fehler_proper_list, (object caller, object obj));
nonreturning_function(extern, fehler_key_odd, (uintC argcount, object caller));
nonreturning_function(extern, fehler_key_badkw, (object fun, object key, object val, object kwlist));
extern void check_value (condition_t errortype, const char * errorstring);
extern object check_posfixnum (object obj);
extern object check_string (object obj);
extern object check_fpointer (object obj, bool restart_p);
extern object check_uint8 (object obj);
extern object check_sint8 (object obj);
extern object check_uint16 (object obj);
extern object check_sint16 (object obj);
extern object check_uint32 (object obj);
extern object check_sint32 (object obj);
extern object check_uint64 (object obj);
extern object check_sint64 (object obj);
extern object check_uint (object obj);
extern object check_sint (object obj);
extern object check_ulong (object obj);
extern object check_slong (object obj);
extern object check_sfloat (object obj);
extern object check_dfloat (object obj);
extern double to_double (object obj);
extern int to_int (object obj);
extern object find_package (object string);
extern uintBWL intern (object string, object pack, object* sym_);
extern object intern_keyword (object string);
extern object object_out (object obj);
#define OBJECT_OUT(obj,label) (printf("[%s:%d] %s: %s:\n",__FILE__,__LINE__,STRING(obj),label),  obj=object_out(obj))
typedef enum { DIRECTION_PROBE=0, DIRECTION_INPUT=1, DIRECTION_INPUT_IMMUTABLE=3, DIRECTION_OUTPUT=4, DIRECTION_IO=5} direction_t;
extern direction_t check_direction (object dir);
typedef enum { IF_DOES_NOT_EXIST_UNBOUND, IF_DOES_NOT_EXIST_ERROR, IF_DOES_NOT_EXIST_NIL, IF_DOES_NOT_EXIST_CREATE } if_does_not_exist_t;
extern if_does_not_exist_t check_if_does_not_exist (object if_not_exist);
extern object if_does_not_exist_symbol (if_does_not_exist_t if_not_exist);
typedef enum { IF_EXISTS_UNBOUND, IF_EXISTS_ERROR, IF_EXISTS_NIL, IF_EXISTS_RENAME, IF_EXISTS_RENAME_AND_DELETE, IF_EXISTS_SUPERSEDE, IF_EXISTS_APPEND, IF_EXISTS_OVERWRITE } if_exists_t;
extern if_exists_t check_if_exists (object if_exists);
extern object if_exists_symbol (if_exists_t if_exists);
#include <time.h>
extern object convert_time_to_universal (const time_t* time);
#define UNIX_LISP_TIME_DIFF 2208988800UL
#define Handle uintW
extern Handle handle_dup (Handle old_handle, Handle new_handle);
extern Handle stream_lend_handle (object stream, bool inputp, int * handletype);
extern uintL read_byte_array (const gcv_object_t* stream_, const gcv_object_t* bytearray_, uintL start, uintL len, bool no_hang);
extern uintL write_byte_array (const gcv_object_t* stream_, const gcv_object_t* bytearray_, uintL start, uintL len, bool no_hang);
extern void builtin_stream_close (const gcv_object_t* stream_);
extern object file_stream_truename (object s);
extern object open_file_stream_handle (object stream, Handle *fd);
extern int write_helper (Handle fd, const void* buf, int nbyte, bool no_hang);
extern int read_helper (Handle fd, void* buf, int nbyte, bool no_hang);
extern object addr_to_string (short type, char *addr);
extern struct hostent* resolve_host (object arg);
#define strm_buffered_bufflen 4096
extern object L_to_I (sint32 wert);
extern object UL_to_I (uintL wert);
extern object L2_to_I (sint32 wert_hi, uint32 wert_lo);
extern object UL2_to_I (uint32 wert_hi, uint32 wert_lo);
#define uint8_to_I(val)  fixnum((uint8)(val))
#define sint8_to_I(val)  L_to_I((sint32)(sint8)(val))
#define uint16_to_I(val)  fixnum((uint16)(val))
#define sint16_to_I(val)  L_to_I((sint32)(sint16)(val))
#define uint32_to_I(val)  UL_to_I((uint32)(val))
#define sint32_to_I(val)  L_to_I((sint32)(val))
#define uint64_to_I(val)  UL2_to_I((uint32)((val)>>32),(uint32)(val))
#define sint64_to_I(val)  L2_to_I((sint32)((val)>>32),(uint32)(val))
#define uint_to_I(val)  uint32_to_I(val)
#define sint_to_I(val)  sint32_to_I(val)
#define ulong_to_I(val)  uint32_to_I(val)
#define slong_to_I(val)  sint32_to_I(val)
extern uintL I_to_UL (object obj);
extern sintL I_to_L (object obj);
extern uint64 I_to_UQ (object obj);
extern sint64 I_to_Q (object obj);
#define I_to_uint8(obj)  (uint8)(as_oint(obj) >> 7)
#define I_to_sint8(obj)  (sint8)(as_oint(obj) >> 7)
#define I_to_uint16(obj)  (uint16)(as_oint(obj) >> 7)
#define I_to_sint16(obj)  (sint16)(as_oint(obj) >> 7)
#define I_to_uint32(obj)  I_to_UL(obj)
#define I_to_sint32(obj)  I_to_L(obj)
#define I_to_uint64(obj)  I_to_UQ(obj)
#define I_to_sint64(obj)  I_to_Q(obj)
#define I_to_uint  I_to_uint32
#define I_to_sint  I_to_sint32
#define I_to_ulong  I_to_uint32
#define I_to_slong  I_to_sint32
extern object c_float_to_FF (const ffloatjanus* val_);
extern void FF_to_c_float (object obj, ffloatjanus* val_);
extern object c_double_to_DF (const dfloatjanus* val_);
extern void DF_to_c_double (object obj, dfloatjanus* val_);
#define FOREIGN void*
typedef struct { XRECORD_HEADER void* fp_pointer;} * Fpointer;
#define fpointerp(obj) (orecordp(obj) && (Record_type(obj) == 48))
#define TheFpointer(obj)  ((Fpointer)(ngci_pointable(obj)-1))
extern object allocate_fpointer (FOREIGN foreign);
#define fp_validp(ptr)  ((record_flags(ptr) & bit(7)) == 0)
extern void* my_malloc (size_t size);
#define unused (void)
enum { seclass_foldable, seclass_no_se, seclass_read, seclass_write, seclass_default};
#define DEFMODULE(module_name,package_name)
#define DEFUN(funname,lambdalist,signature) LISPFUN signature
#define DEFUNF DEFUN
#define DEFUNN DEFUN
#define DEFUNR DEFUN
#define DEFUNW DEFUN
#define DEFUND DEFUN
#define DEFVAR(varname)

#endif /* _CLISP_H */

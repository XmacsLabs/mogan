
/** \file basic.hpp
 *  \copyright GPLv3
 *  \details defines basic types and macros used throughout the kernel.
 *  \author Joris van der Hoeven
 *  \date   1999
 */

#ifndef BASIC_H
#define BASIC_H

#include "fast_alloc.hpp"
#include <math.h>

/**
 * @brief Macro used for template specialization of less than and greater than
 * operators.
 */
#define LESSGTR <>

/**
 * @brief Debugging macro used to disable debugging output.
 */
#define TM_DEBUG(x)

typedef int                    SI;
typedef unsigned int           SN;
typedef short                  HI;
typedef unsigned short         HN;
typedef char                   QI;
typedef unsigned char          QN;
typedef long long int          DI;
typedef unsigned long long int DN;
typedef void*                  pointer;
typedef unsigned int           color;

#define MAX_SI 0x7fffffff

/**
 * @brief Macro defining the minimum value of a signed integer.
 */
#define MIN_SI 0x80000000

/******************************************************************************
 * debugging
 ******************************************************************************/

#if (defined __SUNPRO_CC || defined __clang__ || defined(_WIN32) ||            \
     defined(_WIN64))
#define STACK_NEW_ARRAY(name, T, size) T* name= tm_new_array<T> (size)
#define STACK_DELETE_ARRAY(name) tm_delete_array (name)
#else
#define STACK_NEW_ARRAY(name, T, size) T name[size]
#define STACK_DELETE_ARRAY(name)
#endif

/**
 * @brief Macro used to enable or disable the use of exceptions.
 */
#define USE_EXCEPTIONS

/**
 * @brief Function used to handle a TM failure by displaying an error message
 * and exiting the program.
 *
 * @param msg The error message to display.
 */
void tm_failure (const char* msg);
#ifdef USE_EXCEPTIONS

/**
 * @brief Global variable used to store the current exception message.
 */
extern string the_exception;

/**
 * @brief Function used to throw an exception with a specified error message.
 *
 * @param msg The error message to throw with the exception.
 */
void tm_throw (const char* msg);

/**
 * @brief Function used to handle exceptions by displaying the exception message
 * and exiting the program.
 */
void handle_exceptions ();

/**
 * @brief Macro used to assert that a condition is true, and throw an exception
 * with an error message if the condition is false.
 */
#define ASSERT(cond, msg)                                                      \
  {                                                                            \
    if (!(cond)) tm_throw (msg);                                               \
  }

/**
 * @brief Macro used to throw an exception with a specified error message.
 */
#define TM_FAILED(msg)                                                         \
  { tm_throw (msg); }
#else
#ifdef DEBUG_ASSERT
#include <assert.h>

/**
 * @brief Macro used to assert that a condition is true, and print an error
 * message and exit the program if the condition is false.
 */
#define ASSERT(cond, msg)                                                      \
  {                                                                            \
    if (!(cond)) {                                                             \
      tm_failure (msg);                                                        \
      assert (cond);                                                           \
    }                                                                          \
  }

/**
 * @brief Macro used to print an error message and exit the program.
 */
#define TM_FAILED(msg)                                                         \
  {                                                                            \
    tm_failure (msg);                                                          \
    assert (false);                                                            \
  }
#else

/**
 * @brief Macro used to assert that a condition is true, and print an error
 * message and exit the program if the condition is false.
 */
#define ASSERT(cond, msg)                                                      \
  {                                                                            \
    if (!(cond)) {                                                             \
      tm_failure (msg);                                                        \
    }                                                                          \
  }

/**
 * @brief Macro used to print an error message and exit the program.
 */
#define TM_FAILED(msg)                                                         \
  { tm_failure (msg); }
#endif
#endif

/******************************************************************************
 * miscellaneous routines
 ******************************************************************************/

/**
 * @brief Returns the minimum of two signed integers.
 *
 * @param i The first integer.
 * @param j The second integer.
 * @return The smaller of the two integers.
 */
inline SI
min (SI i, SI j) {
  if (i < j) return i;
  else return j;
}

/**
 * @brief Returns the maximum of two signed integers.
 *
 * @param i The first integer.
 * @param j The second integer.
 * @return The larger of the two integers.
 */
inline SI
max (SI i, SI j) {
  if (i > j) return i;
  else return j;
}

/**
 * @brief Returns the minimum of two signed integers.
 *
 * @param i The first integer.
 * @param j The second integer.
 * @return The smaller of the two integers.
 */
inline DI
min (DI i, DI j) {
  if (i < j) return i;
  else return j;
}

/**
 * @brief Returns the maximum of two signed integers.
 *
 * @param i The first integer.
 * @param j The second integer.
 * @return The larger of the two integers.
 */
inline DI
max (DI i, DI j) {
  if (i > j) return i;
  else return j;
}

/**
 * @brief Returns the minimum of two doubles.
 *
 * @param i The first double.
 * @param j The second double.
 * @return The smaller of the two doubles.
 */
inline double
min (double i, double j) {
  if (i < j) return i;
  else return j;
}

/**
 * @brief Returns the maximum of two doubles.
 *
 * @param i The first double.
 * @param j The second double.
 * @return The larger of the two doubles.
 */
inline double
max (double i, double j) {
  if (i > j) return i;
  else return j;
}

/**
 * @brief Hashes an integer.
 *
 * @param i The integer to hash.
 * @return The hashed value of the integer.
 */
inline int
hash (int i) {
  return i;
}

/**
 * @brief Hashes a long integer.
 *
 * @param i The long integer to hash.
 * @return The hashed value of the long integer.
 */
inline int
hash (long int i) {
  return (int) i;
}

/**
 * @brief Hashes a double integer.
 *
 * @param i The double integer to hash.
 * @return The hashed value of the double integer.
 */
inline int
hash (DI i) {
  return (int) i;
}

/**
 * @brief Hashes an unsigned integer.
 *
 * @param i The unsigned integer to hash.
 * @return The hashed value of the unsigned integer.
 */
inline int
hash (unsigned int i) {
  return i;
}

/**
 * @brief Hashes an unsigned long integer.
 *
 * @param i The unsigned long integer to hash.
 * @return The hashed value of the unsigned long integer.
 */
inline int
hash (unsigned long int i) {
  return (int) i;
}

/**
 * @brief Hashes a double number.
 *
 * @param x The double number to hash.
 * @return The hashed value of the double number.
 */
inline int
hash (DN i) {
  return (int) i;
}

/**
 * @brief Hashes a float number.
 *
 * @param x The float numberto hash.
 * @return The hashed value of the float number.
 */
inline int
hash (float x) {
  union {
    int   n;
    float d;
  } u;
  u.d= x;
  return u.n & 0xffffffff;
}

/**
 * @brief Hashes a double number.
 *
 * @param x The double number to hash.
 * @return The hashed value of the double number.
 */
inline int
hash (double x) {
  union {
    DI     n;
    double d;
  } u;
  u.d= x;
  return (int) (u.n ^ (u.n >> 32));
}

/**
 * @brief Returns a copy of an integer.
 *
 * @param x The integer to copy.
 * @return A copy of the integer.
 */
inline int
copy (int x) {
  return x;
}

/**
 * @brief Converts a double to a signed integer, rounding to the nearest
 * integer.
 *
 * @param x The double to convert.
 * @return The converted signed integer.
 */
inline SI
as_int (double x) {
  return (SI) floor (x + 0.5);
}

/**
 * @brief Rounds a double to the nearest integer.
 *
 * @param x The double to round.
 * @return The rounded double.
 */
inline double
tm_round (double x) {
  return floor (x + 0.5);
}

/**
 * @brief Computes a hash value for a pointer.
 *
 * @param ptr The pointer to hash.
 * @return The hash value.
 */
int hash (pointer ptr);

/**
 * @brief Enumeration of display control options.
 */
enum display_control { INDENT, UNINDENT, HRULE, LF };

/**
 * @brief Output operator for display control options.
 *
 * @param out The output stream to write to.
 * @param ctrl The display control option.
 * @return The output stream.
 */
tm_ostream& operator<< (tm_ostream& out, display_control ctrl);

/**
 * @brief Helper struct for type identification and initialization.
 *
 * @tparam T The type to help identify and initialize.
 */
template <typename T> struct type_helper {
  static int      id;   ///< The type identifier.
  static T        init; ///< The initialized value of the type.
  static inline T init_val () {
    return T ();
  } ///< Returns the initialized value of the type.
};

/**
 * @brief Generates a new type identifier.
 *
 * @return The new type identifier.
 */
int new_type_identifier ();

/**
 * @brief Initializes the type identifier and initialized value for a
 * giventemplate type T.
 *
 * @tparam T The type to initialize.
 */
template <typename T> int type_helper<T>::id= new_type_identifier ();
///< The type identifier for type T.
template <typename T> T type_helper<T>::init= T ();
///< The initialized value for type T.

/******************************************************************************
 * concrete and abstract base structures
 ******************************************************************************/

/**
 * @brief Global variable holding the number of concrete structures currently
 * active.
 */
extern int concrete_count;

/**
 * @brief Structure representing a concrete object with a reference count.
 */
struct concrete_struct {
  int ref_count; ///< The reference count for the concrete object.

  /**
   * @brief Default constructor for the concrete object. Increments the
   * reference count.
   */
  inline concrete_struct () : ref_count (1) { TM_DEBUG (concrete_count++); }

  /**
   * @brief Virtual destructor for the concrete object. Decrements the reference
   * count.
   */
  virtual inline ~concrete_struct () { TM_DEBUG (concrete_count--); }
};

/**
 * @brief Global variable holding the number of abstract structures currently
 * active.
 */
extern int abstract_count;

/**
 * @brief Structure representing an abstract object with a reference count.
 */
struct abstract_struct {
  int ref_count; ///< The reference count for the abstract object.

  /**
   * @brief Default constructor for the abstract object. Sets the reference
   * count to 0.
   */
  inline abstract_struct () : ref_count (0) { TM_DEBUG (abstract_count++); }

  /**
   * @brief Virtual destructor for the abstract object. Decrements the reference
   * count.
   */
  virtual inline ~abstract_struct () { TM_DEBUG (abstract_count--); }
};

/******************************************************************************
 * indirect structures
 ******************************************************************************/

/**
 * @brief Macro used to increment the reference count for a structure object.
 *
 * @param R The structure object to increment the reference count for.
 */
#define INC_COUNT(R)                                                           \
  { (R)->ref_count++; }

/**
 * @brief Macro used to decrement the reference count for a structure object and
 * delete it if the count reaches 0.
 *
 * @param R The structure object to decrement the reference count for and delete
 * if necessary.
 */
#define DEC_COUNT(R)                                                           \
  {                                                                            \
    if (0 == --((R)->ref_count)) {                                             \
      tm_delete (R);                                                           \
    }                                                                          \
  }
// #define DEC_COUNT(R) { if(0==--((R)->ref_count)) { tm_delete (R); R=NULL;}}

/**
 * @brief Macro used to increment the reference count for a structure object,
 * only if the object is not NULL.
 *
 * @param R The structure object to increment the reference count for.
 */
#define INC_COUNT_NULL(R)                                                      \
  {                                                                            \
    if ((R) != NULL) (R)->ref_count++;                                         \
  }
/*#define DEC_COUNT_NULL(R) \
  { if ((R)!=NULL && 0==--((R)->ref_count)) { tm_delete (R); } } */

/**
 * @brief Macro used to decrement the reference count for a structure object and
 * delete it if the count reaches 0, only if the object is not NULL.
 *
 * @param R The structure object to decrement the reference count for and delete
 * if necessary.
 */
#define DEC_COUNT_NULL(R)                                                      \
  {                                                                            \
    if ((R) != NULL && 0 == --((R)->ref_count)) {                              \
      tm_delete (R);                                                           \
      R= NULL;                                                                 \
    }                                                                          \
  }

// concrete

/**
 * @brief Macro used to define a concrete smart pointer with reference counting.
 *
 * The PTR parameter should be a valid identifier that will be used as the name
 * of the smart pointer type.
 *
 * The macro defines the following:
 * - An opaque pointer to the implementation class, named PTR##_rep;
 * - A public copy constructor and destructor for the smart pointer, and a
 * pointer dereference operator;
 * - A public assignment operator that decrements the reference count for the
 * old object and increments the reference count for the new object.
 *
 * @param PTR The name of the concrete smart pointer type to be defined.
 */
#define CONCRETE(PTR)                                                          \
  PTR##_rep* rep;                                                              \
                                                                               \
public:                                                                        \
  inline PTR (const PTR&);                                                     \
  inline ~PTR ();                                                              \
  inline PTR##_rep* operator->();                                              \
  inline PTR&       operator= (PTR x)

/**
 * @brief Macro used to define the implementation of a concrete smart pointer.
 *
 * This macro defines the following:
 * - A copy constructor that increments the reference count for the new object;
 * - A destructor that decrements the reference count for the old object;
 * - A pointer dereference operator that returns the implementation pointer;
 * - An assignment operator that decrements the reference count for the old
 * object, increments the reference count for the new object, and updates the
 * implementation pointer.
 *
 * @param PTR The name of the concrete smart pointer type to be defined.
 */
#define CONCRETE_CODE(PTR)                                                     \
  inline PTR::PTR (const PTR& x) : rep (x.rep) { INC_COUNT (this->rep); }      \
  inline PTR::~PTR () { DEC_COUNT (this->rep); }                               \
  inline PTR##_rep* PTR::operator->() { return rep; }                          \
  inline PTR&       PTR::operator= (PTR x) {                                   \
    INC_COUNT (x.rep);                                                   \
    DEC_COUNT (this->rep);                                               \
    this->rep= x.rep;                                                    \
    return *this;                                                        \
  }

// definition for 1 parameter template classes

/**
 * @brief Macro used to define a concrete smart pointer with reference counting
 * for a single template parameter.
 *
 * The PTR parameter should be a valid identifier that will be used as the name
 * of the smart pointer type. The T parameter should be a valid template
 * parameter that will be used as the type of the managed object.
 *
 * The macro defines the following:
 * - An opaque pointer to the implementation class, named PTR##_rep<T>;
 * - A public copy constructor and destructor for the smart pointer, and a
 * pointer dereference operator;
 * - A public assignment operator that decrements the reference count for the
 * old object and increments the reference count for the new object.
 *
 * @param PTR The name of the concrete smart pointer type to be defined.
 * @param T The type of the managed object.
 */
#define CONCRETE_TEMPLATE(PTR, T)                                              \
  PTR##_rep<T>* rep;                                                           \
                                                                               \
public:                                                                        \
  inline PTR (const PTR<T>&);                                                  \
  inline ~PTR ();                                                              \
  inline PTR##_rep<T>* operator->();                                           \
  inline PTR<T>&       operator= (PTR<T> x)

/**
 * @brief Macro used to define the implementation of a concrete smart pointer
 * with reference counting for a single template parameter.
 *
 * This macro defines the following:
 * - A copy constructor that increments the reference count for the new object;
 * - A destructor that decrements the reference count for the old object;
 * - A pointer dereference operator that returns the implementation pointer;
 * - An assignment operator that decrements the reference count for the old
 * object, increments the reference count for the new object, and updates the
 * implementation pointer.
 *
 * @param PTR The name of the concrete smart pointer type to be defined.
 * @param TT The template parameter type.
 * @param T The type of the managed object.
 */
#define CONCRETE_TEMPLATE_CODE(PTR, TT, T)                                     \
  template <TT T> inline PTR<T>::PTR (const PTR<T>& x) : rep (x.rep) {         \
    INC_COUNT (this->rep);                                                     \
  }                                                                            \
  template <TT T> inline PTR<T>::~PTR () { DEC_COUNT (this->rep); }            \
  template <TT T> inline PTR##_rep<T>* PTR<T>::operator->() {                  \
    return this->rep;                                                          \
  }                                                                            \
  template <TT T> inline PTR<T>& PTR<T>::operator= (PTR<T> x) {                \
    INC_COUNT (x.rep);                                                         \
    DEC_COUNT (this->rep);                                                     \
    this->rep= x.rep;                                                          \
    return *this;                                                              \
  }

// definition for 2 parameter template classes

/**
 * @brief Macro used to define a concrete smart pointer with reference counting
 * for two template parameters.
 *
 * The PTR parameter should be a valid identifier that will be used as the name
 * of the smart pointer type. The T1 and T2 parameters should be valid template
 * parameters that will be used as the types of the managed objects.
 *
 * The macro defines the following:
 * - An opaque pointer to the implementation class, named PTR##_rep<T1, T2>;
 * - A public copy constructor and destructor for the smart pointer, and a
 * pointer dereference operator;
 * - A public assignment operator that decrements the reference count for the
 * old object and increments the reference count for the new object.
 *
 * @param PTR The name of the concrete smart pointer type to be defined.
 * @param T1 The first type of the managed object.
 * @param T2 The second type of the managed object.
 */
#define CONCRETE_TEMPLATE_2(PTR, T1, T2)                                       \
  PTR##_rep<T1, T2>* rep;                                                      \
                                                                               \
public:                                                                        \
  inline PTR (const PTR<T1, T2>&);                                             \
  inline ~PTR ();                                                              \
  inline PTR##_rep<T1, T2>* operator->();                                      \
  inline PTR<T1, T2>&       operator= (PTR<T1, T2> x)

/**
 * @brief Macro used to define the implementation of a concrete smart pointer
 * with reference counting for two template parameters.
 *
 * This macro defines the following:
 * - A copy constructor that increments the reference count for the new object;
 * - A destructor that decrements the reference count for the old object;
 * - A pointer dereference operator that returns the implementation pointer;
 * - An assignment operator that decrements the reference count for the old
 * object, increments the reference count for the new object, and updates the
 * implementation pointer.
 *
 * @param PTR The name of the concrete smart pointer type to be defined.
 * @param TT1 The first template parameter type.
 * @param T1 The first type of the managed object.
 * @param TT2 The second template parameter type.
 * @param T2 The second type of the managed object.
 */
#define CONCRETE_TEMPLATE_2_CODE(PTR, TT1, T1, TT2, T2)                        \
  template <TT1 T1, TT2 T2>                                                    \
  inline PTR<T1, T2>::PTR (const PTR<T1, T2>& x) : rep (x.rep) {               \
    INC_COUNT (this->rep);                                                     \
  }                                                                            \
  template <TT1 T1, TT2 T2> inline PTR<T1, T2>::~PTR () {                      \
    DEC_COUNT (this->rep);                                                     \
  }                                                                            \
  template <TT1 T1, TT2 T2>                                                    \
  inline PTR##_rep<T1, T2>* PTR<T1, T2>::operator->() {                        \
    return this->rep;                                                          \
  }                                                                            \
  template <TT1 T1, TT2 T2>                                                    \
  inline PTR<T1, T2>& PTR<T1, T2>::operator= (PTR<T1, T2> x) {                 \
    INC_COUNT (x.rep);                                                         \
    DEC_COUNT (this->rep);                                                     \
    this->rep= x.rep;                                                          \
    return *this;                                                              \
  }
// end concrete

// abstract

/**
 * @brief Macro to define an abstract pointer type.
 *
 * @param PTR The name of the abstract pointer type to define.
 */
#define ABSTRACT(PTR)                                                          \
  CONCRETE (PTR);                                                              \
  inline PTR (PTR##_rep*)

/**
 * @brief Macro to define an abstract pointer type with code.
 *
 * @param PTR The name of the abstract pointer type to define.
 */
#define ABSTRACT_CODE(PTR)                                                     \
  CONCRETE_CODE (PTR);                                                         \
  inline PTR::PTR (PTR##_rep* rep2) : rep (rep2) { INC_COUNT (this->rep); }

/**
 * @brief Macro to define a templated abstract pointer type.
 *
 * @param PTR The name of the abstract pointer type to define.
 * @param T The template parameter of the abstract pointer type.
 */
#define ABSTRACT_TEMPLATE(PTR, T)                                              \
  CONCRETE_TEMPLATE (PTR, T);                                                  \
  inline PTR (PTR##_rep<T>*)

/**
 * @brief Macro to define a templated abstract pointer type with code.
 *
 * @param PTR The name of the abstract pointer type to define.
 * @param TT The template parameter type of the concrete implementation.
 * @param T The template parameter of the abstract pointer type.
 */
#define ABSTRACT_TEMPLATE_CODE(PTR, TT, T)                                     \
  CONCRETE_TEMPLATE_CODE (PTR, TT, T);                                         \
  template <TT T> inline PTR<T>::PTR (PTR##_rep<T>* rep2) : rep (rep2) {       \
    INC_COUNT (this->rep);                                                     \
  }
// end abstract

/******************************************************************************
 * null indirect structures
 ******************************************************************************/

// concrete_null

/**
 * @brief Macro to define a concrete null pointer type.
 *
 * @param PTR The name of the concrete null pointer type to define.
 */
#define CONCRETE_NULL(PTR)                                                     \
  CONCRETE (PTR);                                                              \
  inline PTR ();                                                               \
  friend bool is_nil /*LESSGTR*/ (PTR x)

/**
 * @brief Code for the concrete null pointer type.
 *
 * @param PTR The name of the concrete null pointer type.
 */
#define CONCRETE_NULL_CODE(PTR)                                                \
  inline PTR::PTR () : rep (NULL) {}                                           \
  inline PTR::PTR (const PTR& x) : rep (x.rep) { INC_COUNT_NULL (this->rep); } \
  inline PTR::~PTR () { DEC_COUNT_NULL (this->rep); }                          \
  inline PTR##_rep* PTR::operator->() { return this->rep; }                    \
  inline PTR&       PTR::operator= (PTR x) {                                   \
    INC_COUNT_NULL (x.rep);                                              \
    DEC_COUNT_NULL (this->rep);                                          \
    this->rep= x.rep;                                                    \
    return *this;                                                        \
  }                                                                            \
  inline bool is_nil (PTR x) { return x.rep == NULL; }

/**
 * @brief Macro to define a templated concrete null pointer type.
 *
 * @param PTR The name of the templated concrete null pointer type to define.
 * @param T The template parameter type of the concrete null pointer type.
 */
#define CONCRETE_NULL_TEMPLATE(PTR, T)                                         \
  CONCRETE_TEMPLATE (PTR, T);                                                  \
  inline PTR ();                                                               \
  friend bool is_nil LESSGTR (PTR<T> x)

/**
 * @brief Code for the templated concrete null pointer type.
 *
 * @param PTR The name of the templated concrete null pointer type.
 * @param TT The template parameter type of the concrete implementation.
 * @param T The template parameter of the concrete null pointer type.
 */
#define CONCRETE_NULL_TEMPLATE_CODE(PTR, TT, T)                                \
  template <TT T> inline PTR<T>::PTR () : rep (NULL) {}                        \
  template <TT T> inline PTR<T>::PTR (const PTR<T>& x) : rep (x.rep) {         \
    INC_COUNT_NULL (this->rep);                                                \
  }                                                                            \
  template <TT T> inline PTR<T>::~PTR () { DEC_COUNT_NULL (this->rep); }       \
  template <TT T> inline PTR##_rep<T>* PTR<T>::operator->() {                  \
    return this->rep;                                                          \
  }                                                                            \
  template <TT T> inline PTR<T>& PTR<T>::operator= (PTR<T> x) {                \
    INC_COUNT_NULL (x.rep);                                                    \
    DEC_COUNT_NULL (this->rep);                                                \
    this->rep= x.rep;                                                          \
    return *this;                                                              \
  }                                                                            \
  template <TT T> inline bool is_nil (PTR<T> x) { return x.rep == NULL; }

/**
 * @brief Macro for concrete null indirect structure two-template-parameter
 * definition.
 * @param PTR Pointer type of the concrete null indirect structure.
 * @param T1 First template parameter of the concrete null indirect structure.
 * @param T2 Second template parameter of the concrete null indirect structure.
 */
#define CONCRETE_NULL_TEMPLATE_2(PTR, T1, T2)                                  \
  CONCRETE_TEMPLATE_2 (PTR, T1, T2);                                           \
  inline PTR ();                                                               \
  friend bool is_nil LESSGTR (PTR<T1, T2> x)

/**
 * @brief Macro for concrete null indirect structure two-template-parameter code
 * definition.
 * @param PTR Pointer type of the concrete null indirect structure.
 * @param TT1 Template type of the first template parameter of the concrete null
 * indirect structure.
 * @param T1 First template parameter of the concrete null indirect structure.
 * @param TT2 Template type of the second template parameter of the concrete
 * null indirect structure.
 * @param T2 Second template parameter of the concrete null indirect structure.
 */
#define CONCRETE_NULL_TEMPLATE_2_CODE(PTR, TT1, T1, TT2, T2)                   \
  template <TT1 T1, TT2 T2> inline PTR<T1, T2>::PTR () : rep (NULL) {}         \
  template <TT1 T1, TT2 T2>                                                    \
  inline PTR<T1, T2>::PTR (const PTR<T1, T2>& x) : rep (x.rep) {               \
    INC_COUNT_NULL (this->rep);                                                \
  }                                                                            \
  template <TT1 T1, TT2 T2> inline PTR<T1, T2>::~PTR () {                      \
    DEC_COUNT_NULL (this->rep);                                                \
  }                                                                            \
  template <TT1 T1, TT2 T2> PTR##_rep<T1, T2>* PTR<T1, T2>::operator->() {     \
    return this->rep;                                                          \
  }                                                                            \
  template <TT1 T1, TT2 T2>                                                    \
  inline PTR<T1, T2>& PTR<T1, T2>::operator= (PTR<T1, T2> x) {                 \
    INC_COUNT_NULL (x.rep);                                                    \
    DEC_COUNT_NULL (this->rep);                                                \
    this->rep= x.rep;                                                          \
    return *this;                                                              \
  }                                                                            \
  template <TT1 T1, TT2 T2> inline bool is_nil (PTR<T1, T2> x) {               \
    return x.rep == NULL;                                                      \
  }
// end concrete_null

// abstract_null

/**
 * @def ABSTRACT_NULL(PTR)
 * @brief Macro for abstract null indirect structure definition.
 * @param PTR Pointer type of the abstract null indirect structure.
 */
#define ABSTRACT_NULL(PTR)                                                     \
  CONCRETE_NULL (PTR);                                                         \
  inline PTR (PTR##_rep*)

/**
 * @brief Macro for abstract null indirect structure code definition.
 * @param PTR Pointer type of the abstract null indirect structure.
 */
#define ABSTRACT_NULL_CODE(PTR)                                                \
  CONCRETE_NULL_CODE (PTR);                                                    \
  inline PTR::PTR (PTR##_rep* rep2) : rep (rep2) { INC_COUNT_NULL (this->rep); }

/**
 * @brief Macro for abstract null indirect structure template definition.
 * @param PTR Pointer type of the abstract null indirect structure.
 * @param T Template parameter of the abstract null indirect structure.
 */
#define ABSTRACT_NULL_TEMPLATE(PTR, T)                                         \
  CONCRETE_NULL_TEMPLATE (PTR, T);                                             \
  inline PTR (PTR##_rep<T>*)

/**
 * @brief Macro for abstract null indirect structure template code definition.
 * @param PTR Pointer type of the abstract null indirect structure.
 * @param TT Template type of the abstract null indirect structure.
 * @param T Template parameter of the abstract null indirect structure.
 */
#define ABSTRACT_NULL_TEMPLATE_CODE(PTR, TT, T)                                \
  CONCRETE_NULL_TEMPLATE_CODE (PTR, TT, T);                                    \
  template <TT T> inline PTR<T>::PTR (PTR##_rep<T>* rep2) : rep (rep2) {       \
    INC_COUNT (this->rep);                                                     \
  }

/**
 * @brief Macro for abstract null indirect structure two-template-parameter
 * definition.
 * @param PTR Pointer type of the abstract null indirect structure.
 * @param T1 First template parameter of the abstract null indirect structure.
 * @param T2 Second template parameter of the abstract null indirect structure.
 */
#define ABSTRACT_NULL_TEMPLATE_2(PTR, T1, T2)                                  \
  CONCRETE_NULL_TEMPLATE_2 (PTR, T1, T2);                                      \
  inline PTR (PTR##_rep<T1, T2>*)

/**
 * @brief Macro for abstract null indirect structure two-template-parameter code
 * definition.
 * @param PTR Pointer type of the abstract null indirect structure.
 * @param TT1 Template type of the first template parameter of the abstract null
 * indirect structure.
 * @param T1 First template parameter of the abstract null indirect structure.
 * @param TT2 Template type of the second template parameter of the abstract
 * null indirect structure.
 * @param T2 Second template parameter of the abstract null indirect structure.
 */
#define ABSTRACT_NULL_TEMPLATE_2_CODE(PTR, TT1, T1, TT2, T2)                   \
  CONCRETE_NULL_TEMPLATE_2_CODE (PTR, TT1, T1, TT2, T2);                       \
  template <TT1 T1, TT2 T2>                                                    \
  inline PTR<T1, T2>::PTR (PTR##_rep<T1, T2>* rep2) : rep (rep2) {             \
    INC_COUNT (this->rep);                                                     \
  }
// end abstract_null

/******************************************************************************
 * extensions
 ******************************************************************************/

/**
 * @def EXTEND(BASE, PTR)
 * @brief Macro for extension of a base indirect structure with an abstract
 * indirect structure.
 * @param BASE Base indirect structure.
 * @param PTR Abstract indirect structure.
 */
#define EXTEND(BASE, PTR)                                                      \
  ABSTRACT (PTR);                                                              \
  inline PTR (BASE&);                                                          \
  inline operator BASE ()

/**
 * @def EXTEND_CODE(BASE, PTR)
 * @brief Macro for extension of a base indirect structure with an abstract
 * indirect structure code implementation.
 * @param BASE Base indirect structure.
 * @param PTR Abstract indirect structure.
 */
#define EXTEND_CODE(BASE, PTR)                                                 \
  ABSTRACT_CODE (PTR);                                                         \
  inline PTR::PTR (BASE& x) : rep (static_cast<PTR##_rep*> (x.rep)) {          \
    INC_COUNT (this->rep);                                                     \
  }                                                                            \
  inline PTR::operator BASE () { return BASE (this->rep); }
// end extend

// extend_null

/**
 * @def EXTEND_NULL(BASE, PTR)
 * @brief Macro for extension of a base indirect structure with a concrete null
 * indirect structure.
 * @param BASE Base indirect structure.
 * @param PTR Concrete null indirect structure.
 */
#define EXTEND_NULL(BASE, PTR)                                                 \
  ABSTRACT_NULL (PTR);                                                         \
  inline PTR (BASE&);                                                          \
  inline operator BASE ()

/**
 * @def EXTEND_NULL_CODE(BASE, PTR)
 * @brief Macro for extension of a base indirect structure with a concrete null
 * indirect structure code implementation.
 * @param BASE Base indirect structure.
 * @param PTR Concrete null indirect structure.
 */
#define EXTEND_NULL_CODE(BASE, PTR)                                            \
  ABSTRACT_NULL_CODE (PTR);                                                    \
  inline PTR::PTR (BASE& x) : rep (static_cast<PTR##_rep*> (x.rep)) {          \
    INC_COUNT_NULL (this->rep);                                                \
  }                                                                            \
  inline PTR::operator BASE () { return BASE (this->rep); }

/**
 * @def EXTEND_NULL_TEMPLATE(BASE, PTR, T)
 * @brief Macro for extension of a base indirect structure with a concrete null
 * indirect structure with a template parameter.
 * @param BASE Base indirect structure.
 * @param PTR Concrete null indirect structure.
 * @param T Template parameter of the concrete null indirect structure.
 */
#define EXTEND_NULL_TEMPLATE(BASE, PTR, T)                                     \
  ABSTRACT_NULL_TEMPLATE (PTR, T);                                             \
  inline PTR<T> (BASE&);                                                       \
  inline operator BASE ()

/**
 * @def EXTEND_NULL_TEMPLATE_CODE(BASE, PTR, TT, T)
 * @brief Macro for extension of a base indirect structure with a concrete null
 * indirect structure with a template parameter code implementation.
 * @param BASE Base indirect structure.
 * @param PTR Concrete null indirect structure.
 * @param TT Template type of the concrete null indirect structure.
 * @param T Template parameter of the concrete null indirect structure.
 */
#define EXTEND_NULL_TEMPLATE_CODE(BASE, PTR, TT, T)                            \
  ABSTRACT_NULL_TEMPLATE_CODE (PTR, TT, T);                                    \
  template <TT T>                                                              \
  inline PTR<T>::PTR (BASE& x) : rep (static_cast<PTR##_rep<T>*> (x.rep)) {    \
    INC_COUNT_NULL (this->rep);                                                \
  }                                                                            \
  template <TT T> inline PTR<T>::operator BASE () { return BASE (this->rep); }
// end extend_null

#endif // defined BASIC_H


/******************************************************************************
 * MODULE     : git.cpp
 * DESCRIPTION: Interface with Git
 * COPYRIGHT  : (C) 2024    Darcy Shen
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "git.hpp"
#include "file.hpp"
#include "tm_debug.hpp"

#include <git2.h>

string
libgit2_version () {
  return string (LIBGIT2_VERSION);
}

static void
show_git_last_error (int error) {
  const git_error* e= git_error_last ();
  debug_io << "Error " << error << "/" << e->klass << ": " << e->message << LF;
}

string
git_load_blob (string rev, url file_u, url repo_u) {
  if (!is_directory (repo_u)) {
    debug_io << repo_u << " is not a valid directory" << LF;
    return "";
  }

  if (!is_regular (repo_u * file_u)) {
    debug_io << repo_u * file_u << " is not a regular file" << LF;
    return "";
  }

  git_libgit2_init ();

  string          ret;
  git_repository* repo = NULL;
  git_index*      index= NULL;
  git_object*     obj  = NULL;
  const git_blob* blob = NULL;
  int             error;
  c_string        repo_c (as_string (repo_u));
  c_string        rev_c (rev * ":" * as_string (file_u));

  error= git_repository_open (&repo, repo_c);
  if (error) {
    show_git_last_error (error);
    return "";
  }

  error= git_repository_index (&index, repo);
  if (error) {
    show_git_last_error (error);
    return "";
  }

  error= git_revparse_single (&obj, repo, rev_c);
  if (error) {
    show_git_last_error (error);
    return "";
  }

  blob                  = (const git_blob*) obj;
  git_off_t   rawsize   = git_blob_rawsize (blob);
  const void* rawcontent= git_blob_rawcontent (blob);
  const char* buffer    = (const char*) rawcontent;

  ret->resize (rawsize);
  for (git_off_t seek= 0; seek < rawsize; seek++) {
    ret[seek]= (char) buffer[seek];
  }

  git_object_free (obj);
  git_index_free (index);
  git_libgit2_shutdown ();
  return ret;
}

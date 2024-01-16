
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
#include <git2/status.h>

string
libgit2_version () {
  return string (LIBGIT2_VERSION);
}

static void
show_git_last_error (int error) {
#if (LIBGIT2_VER_MAJOR == 0) && (LIBGIT2_VER_MINOR <= 27)
  const git_error* e= giterr_last ();
#else
  const git_error* e= git_error_last ();
#endif
  debug_io << "libgit2 error " << error << "/" << e->klass << ": " << e->message
           << LF;
}

static bool
check_repo_and_file (url& file_u, url& repo_u) {
  if (!is_directory (repo_u)) {
    debug_io << repo_u << " is not a valid directory" << LF;
    return false;
  }
  if (is_rooted (file_u)) {
    if (is_regular (file_u) && descends (file_u, repo_u)) {
      file_u= (delta (repo_u, file_u))[2];
    }
    else {
      debug_io << file_u << " is not a regular file" << LF;
      return false;
    }
  }
  return true;
}

string
git_status_file (url file_u, url repo_u) {
  if (!check_repo_and_file (file_u, repo_u)) return "";

  git_repository* repo= NULL;
  c_string        repo_c (as_string (repo_u));
  c_string        file_c (as_string (file_u));
  int             error;
  unsigned int    status_flags;
  char            istatus, wstatus;

  git_libgit2_init ();
  error= git_repository_open (&repo, repo_c);
  if (error) {
    show_git_last_error (error);
    return "";
  }
  error= git_status_file (&status_flags, repo, file_c);
  if (error) {
    show_git_last_error (error);
    return "";
  }
  istatus= ' ';
  wstatus= ' ';
  if (status_flags == GIT_STATUS_CURRENT) {
    return string (istatus) * string (wstatus);
  }
  if (status_flags & GIT_STATUS_INDEX_NEW) istatus= 'A';
  if (status_flags & GIT_STATUS_INDEX_MODIFIED) istatus= 'M';
  if (status_flags & GIT_STATUS_INDEX_DELETED) istatus= 'D';
  // dead branch because there is no rename detection in git_status_file
  if (status_flags & GIT_STATUS_INDEX_RENAMED) istatus= 'R';
  if (status_flags & GIT_STATUS_INDEX_TYPECHANGE) istatus= 'T';

  if (status_flags & GIT_STATUS_WT_NEW) {
    if (istatus == ' ') istatus= '?';
    wstatus= '?';
  }
  if (status_flags & GIT_STATUS_WT_MODIFIED) wstatus= 'M';
  if (status_flags & GIT_STATUS_WT_DELETED) wstatus= 'D';
  // dead branch because there is no rename detection in git_status_file
  if (status_flags & GIT_STATUS_WT_RENAMED) wstatus= 'R';
  if (status_flags & GIT_STATUS_WT_TYPECHANGE) wstatus= 'T';

  if (status_flags & GIT_STATUS_IGNORED) {
    istatus= '!';
    wstatus= '!';
  }

  git_libgit2_shutdown ();
  return string (istatus) * string (wstatus);
}

string
git_load_blob (string rev, url file_u, url repo_u) {
  if (!check_repo_and_file (file_u, repo_u)) return "";

  if (!is_regular (repo_u * file_u)) {
    debug_io << repo_u * file_u << " is not a regular file" << LF;
    return "";
  }

  string          ret;
  git_repository* repo = NULL;
  git_index*      index= NULL;
  git_object*     obj  = NULL;
  const git_blob* blob = NULL;
  int             error;
  c_string        repo_c (as_string (repo_u));
  c_string        rev_c (rev * ":" * as_string (file_u));

  git_libgit2_init ();
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

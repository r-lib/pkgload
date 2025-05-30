# Remove s4 classes created by this package.
# This is only necessary if the package was loaded with devtools. If the
# package was NOT loaded by devtools, it's not necessary to remove the
# classes this way, and attempting to do so will result in errors.
remove_s4_classes <- function(package) {
  nsenv <- ns_env(package)
  if (is.null(nsenv)) {
    return()
  }

  classes <- methods::getClasses(nsenv, FALSE)
  classes <- sort_s4classes(classes, package)
  lapply(classes, remove_s4_class, package)
}

# Sort S4 classes for hierarchical removal
# Derived classes must be removed **after** their parents.
# This reduces to a topological sorting on the S4 dependency class
# https://en.wikipedia.org/wiki/Topological_sorting
sort_s4classes <- function(classes, package) {
  nsenv <- ns_env(package)

  sorted_classes <- vector(mode = 'character', length = 0)

  ## Return the parent class, if any within domestic classes
  extends_first <- function(x, classes) {
    ext <- methods::extends(methods::getClass(x, where = nsenv))
    parent <- ext[2]
    classes %in% parent
  }

  ## Matrix of classes in columns, extending classes in rows
  extended_classes <- vapply(
    classes,
    extends_first,
    rep(TRUE, length(classes)),
    classes
  )

  if (!is.matrix(extended_classes))
    extended_classes <- as.matrix(extended_classes)

  ## Dynamic set of orphan classes (safe to remove)
  start_idx <- which(apply(extended_classes, 2, sum) == 0)

  while (length(start_idx) > 0) {
    ## add node to sorted list (and remove from pending list)
    i <- start_idx[1]
    start_idx <- utils::tail(start_idx, -1)
    sorted_classes <- c(sorted_classes, classes[i])

    ## check its derived classes if any
    for (j in which(extended_classes[i, ])) {
      extended_classes[i, j] <- FALSE
      if (sum(extended_classes[, j]) == 0) {
        start_idx <- c(start_idx, j)
      }
    }
  }
  if (any(extended_classes)) {
    ## Graph has a cycle. This should not happen
    ## Stop or try to continue?
    idx <- !classes %in% sorted_classes
    sorted_classes <- c(sorted_classes, classes[idx])
  }

  sorted_classes
}

# Remove an s4 class from a package loaded by devtools
#
# For classes loaded with devtools, this is necessary so that R doesn't try to
# modify superclasses that don't have references to this class. For example,
# suppose you have package pkgA with class A, and pkgB with class B, which
# contains A. If pkgB is loaded with load_all(), then class B will have a
# reference to class A, and unloading pkgB the normal way, with
# unloadNamespace("pkgB"), will result in some errors. They happen because R
# will look at B, see that it is a superclass of A, then it will try to modify
# A by removing subclass references to B.
#
# This function sidesteps the problem by modifying B. It finds all the classes
# in B@contains which also have references back to B, then modifies B to keep
# references to those classes, but remove references to all other classes.
# Finally, it removes B. Calling removeClass("B") tells the classes referred to
# in B@contains to remove their references back to B.
#
# It is entirely possible that this code is necessary only because of bugs in
# R's S4 implementation.
#
# @param classname The name of the class.
# @param package The package object which contains the class.
remove_s4_class <- function(classname, package) {
  nsenv <- ns_env(package)

  # Make a copy of the class
  class <- methods::getClassDef(classname, package = package, inherits = FALSE)

  # If the class is not defined in this package do not try to remove it
  if (!identical(class@package, package)) {
    return()
  }

  # Find all the references to classes that (this one contains/extends AND
  # have backreferences to this class) so that R doesn't try to modify them.
  keep_idx <- contains_backrefs(classname, package, class@contains)
  class@contains <- class@contains[keep_idx]

  # Assign the modified class back into the package
  methods::assignClassDef(classname, class, where = nsenv, force = TRUE)

  # Remove the class, ignoring failures due to potentially locked environments.
  tryCatch(
    methods::removeClass(classname, where = nsenv),
    error = function(e) NULL
  )
}


# Given a list of SClassExtension objects, this returns a logical vector of the
# same length. Each element is TRUE if the corresponding object has a reference
# to this class, FALSE otherwise.
contains_backrefs <- function(classname, pkgname, contains) {
  # If class_a in pkg_a has class_b in pkg_b as a subclass, return TRUE,
  # otherwise FALSE.
  has_subclass_ref <- function(class_a, pkg_a, class_b, pkg_b) {
    x <- methods::getClassDef(class_a, package = pkg_a)
    if (is.null(x)) return(FALSE)

    subclass_ref <- x@subclasses[[class_b]]

    if (!is.null(subclass_ref) && subclass_ref@package == pkg_b) {
      return(TRUE)
    }

    FALSE
  }

  if (length(contains) == 0) {
    return()
  }

  # Get a named vector of 'contains', where each item's name is the class,
  # and the value is the package.
  contain_pkgs <- sapply(contains, methods::slot, "package")

  mapply(
    has_subclass_ref,
    names(contain_pkgs),
    contain_pkgs,
    classname,
    pkgname
  )
}

# environment for storing state persistence

the <- new.env(parent = emptyenv())

# `strict` determines whether or not strict processing should be used
# i.e. `strict==TRUE` will stop on non-fatal problems
# `strict=="FALSE"` will warn
the$strict <- TRUE

# `attr_name` is used as the name of the attribute used to store metadata
# for an object
the$attr_name<-"metrdata"

# `filename_suffix` is the suffix added to filenames for saving metadata
# associated with an object
the$filename_suffix<-"_metadata.json"

# `class_name` is the class name used for metrdata objects
the$class_name<-"metrdata"

#' Stop or show warning
#'
#' Internal helper for `metrdata`, either stop with an error or show warning
#' (if strict mode is `FALSE`).
#'
#' @param msg Message to display.
#'
#' @return None.
#'
#' @examples
#' testthat::expect_error(metrdata:::stop_or_warn("problem encountered"))
stop_or_warn <- function(msg) {
  if (the$strict) {
    stop(msg)
  } else {
    warning(msg)
  }
}

#' Append suffix to path
#'
#' Appends the default metadata filename suffix
#' (``r paste0('"', the$filename_suffix,'"')``) to `path`.
#'
#' @param path Path to assign suffix to.
#'
#' @return Path with suffix appended.
#' @export
#' @seealso [get_metrdata_suffix()]
#'
#' @examples
#' append_metrdata_suffix("~/my_path")
append_metrdata_suffix <- function(path) {
  paste0(path, the$filename_suffix)
}


#' Get the filename suffix used for writing metadata sidecar files
#'
#' Returns the filename suffix used for writing metadata sidecar files
#' (``r the$filename_suffix``).
#'
#' @return Returns ``r paste0('"', the$filename_suffix,'"')``.
#' @export
#' @seealso [append_metrdata_suffix()]
#'
#' @examples
#' get_metrdata_suffix()
get_metrdata_suffix <- function() {
  the$class_name
}

#' Get the class name used for `metrdata` objects
#'
#' Returns the class name used for `metrdata` objects (which is
#' ``r the$class_name``).
#'
#' @return Returns ``r paste0('"', the$class_name,'"')``.
#' @export
#'
#' @examples
#' get_metrdata_classname()
get_metrdata_classname <- function() {
  the$class_name
}


#' Get the attribute name used for storing metadata associated with objects
#'
#' Returns the attribute name used for storing `metrdata` objects that are
#' associated with another object as metadata (which is ``r the$attr_name``).
#'
#' @return Returns ``r paste0('"', the$attr_name,'"')``.
#' @export
#'
#' @examples
#' get_metrdata_attrname()
get_metrdata_attrname <- function() {
  the$attr_name
}

#' Set strict behaviour
#'
#' This function enables or disables strict behaviour in this package.
#' Strict behaviour is enabled by default and causes errors for
#' non-critical issues when processing metadata, instead of just warnings.
#'
#' @param strict_status Status to set to.
#'
#' @return None.
#' @export
#'
#' @examples
#' set_strict(FALSE)
#' metrdata:::stop_or_warn("problem encountered") # warning only
#' set_strict(TRUE)
set_strict <- function(strict_status = TRUE) {
  the$strict <- strict_status
}

#' Convert a list into a `metrdata` object
#'
#' Helper function for setting class of an existing list to a `metrdata` object.
#' Meant for package internal use.
#'
#' @param metadata_list List to convert to a `metrdata` object.
#'
#' @return `metadata_list` as a `metrdata` object.
#'
convert_to_metrdata <- function(metadata_list) {
  stopifnot(isa(metadata_list, "list"))
  class(metadata_list) <- c(the$class_name, class(metadata_list))
  metadata_list
}

#' `metrdata` object constructor
#'
#' Constructs a `metrdata` object from provided arguments.
#'
#' The `metrdata` constructor supports multiple patterns of usage.
#'
#' The most basic form is:
#'
#' ``metrdata(`Original data source` = list(Note = "Fictional data", Created = "2024-01"))``
#'
#' where metadata is directly specified is named arguments in the constructor.
#' Note that usage of generic labels such as "Note" in the top-level of the
#' metadata is discouraged as the context may become obfuscated if the metadata
#' is inherited by derived data; instead, top-level descriptive labels are
#' encouraged.
#'
#' A related alternative is to initialise based on a list provided as the first
#' argument in the constructor:
#' ``metrdata(list(`Original data source` = list(Note = "Fictional data", Created = "2024-01")))``
#'
#' The top-level name for the metadata can also be provided directly using the
#' `label` parameter; this gives the same result as the above:
#' `metrdata(label = "Original data source", list(Note = "Fictional data", Created = "2024-01"))`
#'
#' `metrdata` objects also supported inclusion of nesting of other metadata,
#' and directly 'includes' at the top level. The difference between nesting
#' and 'includes' are that nesting refers to the inclusion of other metadata
#' contents under a new heading, while 'includes' refer to direct inclusion
#' of metadata contents in the top-level of the new object. The use of
#' unique descriptive headings at the top-level allows 'includes' to be used
#' with metadata accumulating at different processing steps, without requiring
#' excessive nesting that more reduce readability.
#'
#' @inheritSection preprocess_metadata Inclusion of nested lists
#' @inheritSection include_metrdata Includes
#'
#' @param ... Contents of metadata. Specified a series of named parameters
#'   as metadata; nesting with lists or references to other objects with metadata
#'   or `metrdata` objects allowed. If a single unnamed list is
#'   provided, its contents are converted to a `metrdata` object. Any unnamed
#'   objects with metadata or `metrdata` objects specified will be used as
#'   includes. Any named metadata or `metrdata` objects are nested under the
#'   provided name.
#' @param label Optional label to be provided as name for first argument, e.g
#'   `metrdata(list(), label="arg1")` is the same as `metrdata(arg1 = list())`.
#' @param include A single or list of objects with metadata or `metrdata`
#'   object to include in the top-level of the new object.
#'
#' @return A `metrdata` object.
#' @export
#'
#' @examples
#' # these produce the same result
#' metrdata(`Original data source` = list(Note="Fictional data", Created="2024-01"))
#' metrdata(list(`Original data source` = list(Note="Fictional data", Created="2024-01")))
#' metrdata(label = "Original data source", list(Note="Fictional data", Created="2024-01"))
#'
#' # Nesting; nested metrdata object is included in the newly constructed object
#' # The same result would work if an obj with the associated metadata assign
#' # was passed instead of the metrdata object itself
#' metrdata(
#'   `Original data source` = list(Note="Fictional data", Created="2024-01"),
#'   Toplevel = metrdata(Note="This is nested metadata")
#' )
#'
#' # 'Includes':
#' # Unnamed metrdata objects or objects with metadata are considered 'includes'
#' # Alternatively, these are provided under the include parameter (a list of
#' # these is also supported)
#' # The contents of these are included directly in the top-level of the new
#' # object
#' metrdata(
#'   `Original data source` = list(Note="Fictional data", Created="2024-01"),
#'   metrdata(Note="This is an included metadata"))
#'
#' metrdata(
#'   `Original data source` = list(Note="Fictional data", Created="2024-01"),
#'   include = metrdata(Note="This is an included metadata"))

metrdata <- function(..., label = NULL, include = NULL) {
  args<-list(...)
  if (!is.null(label)) {
    if (length(args)==0)
      stop("cannot apply label, ... is empty")
    if (is.null(names(args)[1]) ||
        is.na(names(args)[1]) ||
        names(args)[1] == "") {
      names(args)[1]<-label
    } else {
      stop("arg[1] has a name alread, cannot apply label")
    }
  }
  if (length(args)==0 && is.null(include)) {
    # empty object
    return(convert_to_metrdata(list()))
  } else if (length(args)==1) {
    # single parameter, is it single, unnamed list?
    if (inherits(args[[1]], "list") && is.null(names(args))) {
      # yes, used that parameter to init the metrdata obj
      # rather than contents of ...
      args<-args[[1]]
    }
  }
  # init metrdata object
  if (is_metrdata(args)) {

    stop_or_warn("object is already a metrdata object")
    return(args)
  }

  stopifnot(isa(args, "list"))

  if (length(args)>0) {
    if (is.null(names(args)))
      names(args) <- ""
    names(args)[is.na(names(args))]<-""

    to_include<-names(args)=="" & sapply(args, has_is_metrdata)
    to_include_list<-args[to_include]
    other_args<-args[!to_include]
  } else {
    other_args<-list()
    to_include_list<-list()
  }

  if (!is.null(include)) {
    if (!isa(include, "list"))
      include<-list(include)
    to_include_list<-c(to_include_list,include)
  }

  included<-include_metrdata(to_include_list)

  initial_metadata<-c(other_args, included)
  preprocess_metadata(initial_metadata)
}

#' Preprocess a list into metadata
#'
#' Preprocess a list into a `metrdata` object.
#' This function is not meant to be called directly; it is an internal helper
#' meant to be called from the `metrdata` constructor.
#'
#' @section Inclusion of nested lists:
#'
#' Child lists are processed when converting to a `metrdata` object by
#' parsing its contents and directly embedding metadata from any children that
#' themselves are `metrdata` objects or have associated metadata.
#'
#' This allows a pattern where an object containing metadata may be referenced
#' when creating metadata for an object.
#'
#' @param metadata_list A `list` to process into a `metrdata` object.
#'
#' @return A `metrdata` object processed from `metadata_list`.
preprocess_metadata <- function(metadata_list) {
  stopifnot(isa(metadata_list, "list"))

  # empty list, no processing needed
  if(length(metadata_list)==0) return(convert_to_metrdata(metadata_list))

  # helper function for recursive processing
  metadata_or_self<-function(obj) {
    if(has_is_metrdata(obj)){
      # if obj is a metrdata_obj or has metadata, use that instead (as list)
      get_metadata(obj, as_list=TRUE)
    } else if(isa(obj, "list")) {
      # if obj is a list, try to recursively call `metadata_or_self`
      result<-lapply(obj, metadata_or_self)
      if ( !is.null(names(result)) ) {
        # sanity check; this ensures we don't end up giving names based on
        # indexes when converting to json
        if (
          any(is.na(names(result))) ||
          any(sapply(names(result), function(x){x==""})) )
          stop_or_warn(
            "partly named list; all items in a list should be named or not named")
      }
      result

    } else {
      # no metadata, not list, use as-is
      obj
    }
  }

  # recursively process list
  processed_metadata<-metadata_or_self(metadata_list)

  if (
    is.null(names(processed_metadata)) ||
    any(is.na(names(processed_metadata))) ||
    any(duplicated(names(processed_metadata))) ||
    any(names(processed_metadata)=="")
    )
    stop_or_warn("duplicated or empty top-level names in metrdata object")

  convert_to_metrdata(processed_metadata)
}


#' Concatenate the metadata from to `metrdata` objects
#'
#' Concatenate the metadata of the two provide `metrdata` objects. Each
#' object must be a `metrdata` object and two objects should not contain
#' the same keys.
#'
#' If the two objects contain the same keys, an error or warning is produced
#' depending on strict mode.
#'
#' This is an internal helper function. Package users should use the
#' `metrdata()` constructor.
#'
#' @param metrdata_obj1 First `metrdata` object to concatenate.
#' @param metrdata_obj2 Second `metrdata` object to concatenate.
#'
#' @return A `metrdata` object representing a concatenation of both source
#'   objects.
cat_metadata <- function(metrdata_obj1, metrdata_obj2) {
  stopifnot(is_metrdata(metrdata_obj1), is_metrdata(metrdata_obj2))
  common_names<-intersect(names(metrdata_obj1), names(metrdata_obj2))
  if (length(common_names) > 0) {
    stop_or_warn(paste0("repeated metadata labels when combining metadata: ",common_names))
  }
  metrdata(c(metrdata_obj1, metrdata_obj2))
}


#' Process `metrdata` 'includes'
#'
#' This is a helper function for processing other `metrdata` objects 'included'
#' when constructing a new `metrdata` object.
#'
#' @section Includes:
#'
#' An 'include' is an object with metadata or a `metrdata` object whose
#' contents is included directly in the new object, i.e. their contents are
#' flattened into the new object.
#'
#' @param include_from List of objects with metadata or `metrdata` objects to
#'   include.
#'
#' @return A new `metrdata` list with contents of all objects in `include_from`
#'   concatenated.
#'
include_metrdata<-function(include_from) {
  #include_from = list(...)

  if (!all(sapply(include_from, has_is_metrdata))) {
    stop("... should include only objects with or which are metrdata ")
  }

  if (is.null(names(include_from)))
    names(include_from) <- ""
  names(include_from)[is.na(names(include_from))]<-""

  Reduce(cat_metadata, mapply(function(name, include) {
      metadata_obj <- get_metadata(include)
      if (name!="") {
        renamed<-list(metadata_obj)
        names(renamed) <- name
        metrdata(list)
      } else {
        metadata_obj
      }
    },
    names(include_from),
    include_from, SIMPLIFY = FALSE)
  )
}

#' Test whether object is a `metrdata` object
#'
#' This function tests if an object is a `metrdata` object.
#'
#' @param this Object to test.
#'
#' @return `TRUE` if `this` is a `metrdata` object, otherwise `FALSE`.
#' @export
#'
#' @examples
#' is_metrdata(metrdata()) # TRUE
#' is_metrdata(list()) # FALSE
is_metrdata <- function(this) {
  any(class(this)==the$class_name)
}

#' Test whether object has metadata or is a `metrdata` object
#'
#' This function tests if an object is itself a `metrdata` object, or
#' has any associated metadata.
#'
#' @param this Object to test.
#'
#' @return `TRUE` if `this` is a `metrdata` object or has metadata,
#'   otherwise `FALSE`.
#' @export
#' @seealso [is_metrdata()] [has_metadata()]
#'
#' @examples
#' obj_metrdata <- metrdata(list(note = "Indexes 1 to 10"))
#' has_is_metrdata(obj_metrdata) # TRUE
#' is_metrdata(list()) # FALSE
#' a_data_frame <- data.frame(index = 1:10)
#' has_is_metrdata(a_data_frame) # FALSE
#' metadata(a_data_frame) <- obj_metrdata
#' has_is_metrdata(a_data_frame) # TRUE
has_is_metrdata <- function(this) {
  is_metrdata(this) || has_metadata(this)
}

#' Get metadata of an object
#'
#' These functions return the metadata of the provided object. An error is
#' produced if the object does not have any associated metadata.
#'
#' The use of `get_metadata()` is preferred as the intention is more explicit;
#' `metadata()` is provided as an alias for completeness.
#'
#' @param obj Object to obtain metadata from. If `obj` is itself a `metrdata`
#'   object, than `obj` is itself returned.
#' @param as_list If `TRUE`, simplify return value to a list, instead of
#'   returning a `metrdata` object.
#'
#' @return `metrdata` object or list containing the metadata of `obj`.
#' @export
#'
#' @examples
#' a_data_frame <- data.frame(index = 1:10)
#' metadata(a_data_frame) <- metrdata(list(note = "Indexes 1 to 10"))
#' get_metadata(a_data_frame)
#' metadata(a_data_frame)
get_metadata <- function(obj, as_list = FALSE) {
  if (is_metrdata(obj)) {
    metrdata_obj<-obj
  } else {
    stopifnot(has_metadata(obj))
    metrdata_obj<-attr(obj, the$attr_name)
    stopifnot(is_metrdata(metrdata_obj))
  }
  if (as_list) {
    # sanity check, metrdata_obj should always inherit from list
    stopifnot(inherits(metrdata_obj,"list"))
    class(metrdata_obj)<-class(list())
  }
  metrdata_obj
}

#' @rdname get_metadata
#' @export
metadata <- get_metadata

#' Check whether object has metadata
#'
#' This function checks whether the provided object has any associated metadata.
#'
#' @param obj Object to test for presence of metadata.
#'
#' @return `TRUE` if object has metadata, otherwise `FALSE`
#' @export
#'
#' @examples
#' a_data_frame <- data.frame(index = 1:10)
#' has_metadata(a_data_frame) # FALSE
#' metadata(a_data_frame) <- metrdata(list(note = "Indexes 1 to 10"))
#' has_metadata(a_data_frame) # TRUE
#' stripped_data_frame <- strip_metadata(a_data_frame)
#' has_metadata(stripped_data_frame) # FALSE
has_metadata <- function(obj) {
  if (is.null(attr(obj, the$attr_name))) {
    FALSE
  } else {
    stopifnot(is_metrdata(attr(obj, the$attr_name)))
    TRUE
  }
}

#' Assign metadata of an object
#'
#' Directly assign the metadata of an object. The object must not already have
#' metadata and the provided value must be a `metrdata` object.
#'
#' The `metrdata` object can be constructed using the `metrdata()` constructor,
#' or using `prepare_metadata(label, metadata)`.
#'
#' @param obj Object to assign metadata to; should not already have metadata.
#' @param value `metrdata` object to assign as metadata.
#' @param replace If `TRUE`, existing metadata in obj may be replaced; defaults
#'   to error if attempting to replace metadata.
#'
#' @return `obj` with metadata.
#' @export
#'
#' @examples
#' a_data_frame <- data.frame(index = 1:10)
#' metadata(a_data_frame) <- metrdata(list(note = "Indexes 1 to 10"))
#' get_metadata(a_data_frame)
#'
#' a_data_frame2 <- data.frame(index = 1:20)
#' a_data_frame2 <- set_metadata(a_data_frame2, metrdata(list(note = "Indexes 1 to 20")))
#' get_metadata(a_data_frame2)
set_metadata <- function(obj, value, replace = FALSE) {
  stopifnot(is_metrdata(value))
  metrdata_obj<-value
  if (!replace && has_metadata(obj))
    stop("obj already has metadata, won't replace")
  if ( any(sapply(names(metrdata_obj), function(x){x==""})) )
    stop_or_warn("empty name in metrdata_obj")
  attr(obj, the$attr_name)<-metrdata_obj
  obj
}

#' @rdname set_metadata
#' @export
`metadata<-` <- function(obj, value) {
  set_metadata(obj, value)
}

#' Remove metadata from an object
#'
#' This function removes metadata from the provided object.
#' An error/warning is produced if the object does not have any associated
#' metadata.
#'
#' @param obj Object to strip metadata from.
#'
#' @return Object without metadata.
#' @export
#'
#' @examples
#' a_data_frame <- data.frame(index = 1:10)
#' metadata(a_data_frame) <- metrdata(list(note = "Indexes 1 to 10"))
#' has_metadata(a_data_frame) # TRUE
#' stripped_data_frame <- strip_metadata(a_data_frame)
#' has_metadata(stripped_data_frame) # FALSE
strip_metadata <- function(obj) {
  if (has_metadata(obj)) {
    attr(obj, the$attr_name)<-NULL
  } else {
    stop_or_warn("obj has no metadata")
  }
  obj
}

#' Read metadata from a sidecar file
#'
#' Metadata in this package is stored in sidecar files in JSON format alongside
#' the data file for which the metadata is associated with. The sidecar file
#' should have the same name as the data file but with the suffix
#' "`r toString(the$filename_suffix)` appended.
#' This function reads metadata (only) from a sidecar file.
#'
#' @param path Path to read metadata from.
#' @param add_suffix If `TRUE`, the standard suffix for metadata sidecar files
#' is automatically appended to `path` before reading.
#' @param ... Additional parameters passed to `jsonlite::read_json` for reading.
#'
#' @return Metadata as a `metrdata` object.
#' @export
#' @seealso [write_metadata()]
#'
#' @examples
#' a_data_frame <- data.frame(index = 1:10)
#' metadata(a_data_frame) <- metrdata(list(note = "Indexes 1 to 10"))
#' tmpfilename<-tempfile()
#' write_metadata(a_data_frame,tmpfilename) # saves to tmpfilename + suffix
#' reread_metadata<-read_metadata(tmpfilename)
#' testthat::expect_equal(get_metadata(a_data_frame), reread_metadata)
#' file.remove(metrdata:::append_metrdata_suffix(tmpfilename))
read_metadata <- function(path, add_suffix = TRUE, ...) {
  if (add_suffix) {
    path <- append_metrdata_suffix(path)
  }
  metrdata_obj<-
    metrdata(jsonlite::read_json(path = path, simplifyVector = TRUE, ...))
  metrdata_obj
}

#' Read a file together with its metadata
#'
#' This series of functions read a file together with its associated metadata.
#' The metadata is stored as a `metrdata` object in the ``r the$attr_name``
#' attributed for the resulting data object.
#'
#' `read_with_metadata()` allows a data file to be read with a user-provided
#' function for reading from `path`.
#'
#' `read_tsv_metadata()` reads the data file from `path` using `readr::read_tsv()`.
#'
#' `read_csv_metadata()` reads the data file from `path` using `readr::read_csv()`.
#'
#' `read_csv2_metadata()` reads the data file from `path` using `readr::read_csv2()`.
#'
#' `read_delim_metadata()` reads the data file from `path` using `readr::read_delim()`.
#'
#' @param path Path to read data from. The path for the metadata sidecar file
#'   is automatically derived by appending the expected suffix to this path.
#' @param read_func For `read_with_metadata`, the user should provide the
#'   function used for reading the data from `path`.
#' @param ... Additional parameters passed to the read function.
#' @param metadata_args Additional parameters passed to `read_metadata()`.
#'
#' @return The data object read using the read function, with metadata read
#'   from the sidecar file attached as an attribute.
#' @export
#' @seealso [read_metadata()] [readr::read_tsv()] [readr::read_csv()]
#'   [readr::read_csv2()] [readr::read_delim()]
#'
#' a_data_frame <- data.frame(index = 1:10)
#' metadata(a_data_frame) <- metrdata(list(note = "Indexes 1 to 10"))
#' tmpfilename<-tempfile()
#' write_with_metadata(a_data_frame, tmpfilename, write_func = write.csv, row.names = FALSE)
#' a_data_frame2<-read_with_metadata(tmpfilename, read_func = read.csv)
#' testthat::expect_equal(a_data_frame, a_data_frame2)
#'
#' write_tsv_metadata(a_data_frame, tmpfilename)
#' a_data_frame2<-read_tsv_metadata(tmpfilename) # reread as tibble, so ignore class
#' testthat::expect_equal(a_data_frame, a_data_frame2, ignore_attr = TRUE)
#' testthat::expect_equal(metadata(a_data_frame), metadata(a_data_frame2))
#'
#' write_csv_metadata(a_data_frame, tmpfilename)
#' a_data_frame3<-read_csv_metadata(tmpfilename)
#' testthat::expect_equal(a_data_frame3, a_data_frame2)
#'
#' write_csv2_metadata(a_data_frame, tmpfilename)
#' a_data_frame4<-read_csv2_metadata(tmpfilename)
#' testthat::expect_equal(a_data_frame4, a_data_frame2)
#'
#' write_delim_metadata(a_data_frame, tmpfilename, delim=",")
#' a_data_frame5<-read_delim_metadata(tmpfilename, delim=",")
#' testthat::expect_equal(a_data_frame5, a_data_frame2)
#'
#' file.remove(tmpfilename)
#' file.remove(metrdata:::append_metrdata_suffix(tmpfilename))
read_with_metadata <- function(path, read_func, ...,
  metadata_args = list()) {
  obj <- read_func(path, ...)
  metrdata_obj <- do.call(read_metadata,
    args = c(list(path, add_suffix = TRUE), metadata_args))
  metadata(obj)<-metrdata_obj
  obj
}

#' @rdname read_with_metadata
#' @export
read_tsv_metadata <- function(path, ..., metadata_args = list()) {
  read_with_metadata(path, read_func = readr::read_tsv, ...)
}

#' @rdname read_with_metadata
#' @export
read_csv_metadata <- function(path, ..., metadata_args = list()) {
  read_with_metadata(path, read_func = readr::read_csv, ...)
}

#' @rdname read_with_metadata
#' @export
read_csv2_metadata <- function(path, ..., metadata_args = list()) {
  read_with_metadata(path, read_func = readr::read_csv2, ...)
}

#' @rdname read_with_metadata
#' @export
read_delim_metadata <- function(path, ..., metadata_args = list()) {
  read_with_metadata(path, read_func = readr::read_delim, ...)
}

#' Write metadata to a sidecar file
#'
#' This function writes metadata (only) to a sidecar file. It can be used to
#' write metadata associated with an object, or a `metrdata` object directly.
#'
#' @param obj Object containing metadata or `metrdata` object to write.
#' @param path  Path to write metadata to.
#' @inheritParams read_metadata
#' @inheritParams jsonlite::toJSON
#' @param ... Additional parameters based to `jsonlite::write_json()` for writing.
#'
#' @return None.
#' @export
#' @seealso [read_metadata()]
#'
#' @inherit read_metadata examples
write_metadata <- function(obj, path, pretty = 2, add_suffix = TRUE, ...) {
  metrdata_obj<-get_metadata(obj)
  if (add_suffix) {
    path <- append_metrdata_suffix(path)
  }
  jsonlite::write_json(metrdata_obj, path = path, pretty = pretty, ...)
}

#' Write a file together with its metadata
#'
#' This series of functions writes an object to file together with its
#' associated metadata.
#' The metadata is written in a JSON sidecar file alongside the data file.
#'
#' `write_with_metadata()` allows a data file to be written with a user-provided
#' function for writing to `path`.
#'
#' `write_tsv_metadata()` writes the data file to `path` using `readr::write_tsv()`.
#'
#' `write_csv_metadata()` writes the data file to `path` using `readr::write_csv()`.
#'
#' `write_csv2_metadata()` writes the data file to `path` using `readr::write_csv2()`.
#'
#' `write_delim_metadata()` writes the data file to `path` using `readr::write_delim()`.
#'
#'
#' @param obj Object to write to file. The object must contain associated
#'   metadata.
#' @param path Path to write data to The path for the metadata sidecar file
#'   is automatically derived by appending the expected suffix to this path.
#' @param write_func For `write_with_metadata`, the user should provide the
#'   function used for writing the data to `path`.
#' @param ... Additional parameters passed to the write function.
#' @param metadata_args Additional parameters passed to `write_metadata()`.
#'
#' @return None.
#' @export
#' @seealso [write_metadata()] [readr::write_tsv()] [readr::write_csv()]
#'   [readr::write_csv2()] [readr::write_delim()]
#'
#' @inherit read_with_metadata examples
write_with_metadata <- function(obj, path, write_func, ...,
  metadata_args = list(pretty = 2)) {
  stopifnot(has_metadata(obj))
  write_func(obj, path, ...)
  do.call(write_metadata,
    args = c(list(obj, path, add_suffix = TRUE), metadata_args))
}


#' @rdname write_with_metadata
#' @export
write_tsv_metadata <- function(obj, path, ...,
  metadata_args = list(pretty = 2)) {
  write_with_metadata(obj, path, write_func = readr::write_tsv,
    metadata_args = metadata_args, ...)
}

#' @rdname write_with_metadata
#' @export
write_csv_metadata <- function(obj, path, ...,
  metadata_args = list(pretty = 2)) {
  write_with_metadata(obj, path, write_func = readr::write_csv,
    metadata_args = metadata_args, ...)
}

#' @rdname write_with_metadata
#' @export
write_csv2_metadata <- function(obj, path, ...,
  metadata_args = list(pretty = 2)) {
  write_with_metadata(obj, path, write_func = readr::write_csv2,
    metadata_args = metadata_args, ...)
}

#' @rdname write_with_metadata
#' @export
write_delim_metadata <- function(obj, path, ...,
  metadata_args = list(pretty = 2)) {
  write_with_metadata(obj, path, write_func = readr::write_delim,
    metadata_args = metadata_args, ...)
}

#' Combine metadata to an existing object
#'
#' This is the internal helper for append/prepend metadata.
#'
#' @param obj Object or `metrdata` object to append metadata to.
#' @param metrdata_obj `metrdata` object to append to existing object.
#' @param prepend If `TRUE`, `metrdata_obj` is
#'   prepended to head of existing metadata;
#'   otherwise `metrdata_obj` is appended to tail of existing metadata.
#'
#' @return `obj` with `metrdata_obj` appended to its metadata.
combine_metadata <- function(obj, metrdata_obj, prepend = FALSE) {
  if (has_is_metrdata(obj)) {
    include <- get_metadata(obj)
  } else {
    include <- NULL
  }
  if (prepend) {
    combined<-metrdata(include = list(metrdata_obj, include))
  } else {
    combined<-metrdata(include = list(include, metrdata_obj))
  }


  if (is_metrdata(obj)) {
    combined
  } else {
    set_metadata(
      obj, replace = TRUE, combined
    )
  }
}

#' Append/prepend metadata to an existing object
#'
#' Append or prepend additional metadata to an existing object with
#' metadata or `metrdata` object. If the the existing object does not already
#' have metadata, the provided metadata is used directly as the object's
#' metadata.
#'
#' @inheritParams combine_metadata
#' @export
#'
#' @examples
#' append_metadata(
#'   set_metadata(data.frame(index = 1:10),
#'     metrdata(`Original data source` = list(Note="Fictional data", Created="2024-01"))
#'     ),
#'   metrdata(Note="This is an included metadata"))
#' prepend_metadata(
#'   set_metadata(data.frame(index = 1:10),
#'     metrdata(`Original data source` = list(Note="Fictional data", Created="2024-01"))
#'     ),
#'   metrdata(Note="This is an included metadata"))
prepend_metadata <- function(obj, metrdata_obj) {
  combine_metadata(obj, metrdata_obj, prepend = TRUE)
}

#' @rdname prepend_metadata
#' @export
append_metadata <- function(obj, metrdata_obj) {
  combine_metadata(obj, metrdata_obj, prepend = FALSE)
}



#' Edit object metadata
#'
#' Edits the metadata of an object or a `metrdata` object.
#'
#' @param obj Object with metadata or `metrdata` object to edit.
#' @param edit_path Search path for edit, this a vector or list giving the
#'  keys to look up to get to the item to edit, e.g. `c("lvl1a","lvl1a2a"`)
#'  will lookup `$lvl1a$lvl1a2a`. A list can be used with a numeric index
#'  to lookup any items by index.
#' @param edit_command Type of edit to perform. Supported commands are `<-` or
#'   `=` for assignment; `-` for deletion; `+` for appending (to a list or
#'   vector); `+0` for prepending; `str+` for appending to a string;
#'   `str+0` for prepending to a string; or an function called with
#'   `(cur_value, edit_value, ...)`.
#' @param edit_value Value to assign, append, or prepend. Not used for deletion.
#' @param ... Additional parameters passed to function given in edit_command.
#'
#' @return A copy of obj with metadata edited.
#' @export
#'
#' @examples
#' my_data<-set_metadata(
#'   data.frame(index=1:10),
#'   metrdata(
#'     lvl1a = list(lvl1a2a = "item1", lvl1a2b="item2", lvl1a2c = c(1,2,3)),
#'     lvl1b = list(lvl1b2a = list(lvl1b2a3a = "xyz"))
#'   )
#' )
#'
#' # edit metadata of object
#' my_data<-edit_metadata(
#'   my_data,
#'   c("lvl1a","lvl1a2d"),"+","item4"
#' )
#' metadata(my_data)
edit_metadata <- function(obj,
  edit_path,
  edit_command = c("<-","=","-","+","+0","str+","str+0",
    function(cur_value, edit_value){}),
  edit_value = NULL, ...) {
  if (length(edit_path)<1) {
    stop("need at least 1 element in edit_path to edit")
  }

  if (!is.function(edit_command))
    edit_command <- unlist(match.arg(edit_command))

  metrdata_obj<-get_metadata(obj)
  cur_level<-metrdata_obj

  if (is.character(edit_command) && edit_command=="-") {
    if (!is.null(edit_value))
      warning("edit_value ignored for element deletion (edit command -)")
  }

  if (is.character(edit_command) && (edit_command=="+" || edit_command=="+0")) {
    edit_newlevel <- edit_path[[length(edit_path)]]
    edit_path <- edit_path[-length(edit_path)]
  } else {
    edit_newlevel <- NULL
  }

  back_path<-list()
  depth<-0
  for (element in edit_path) {
    back_path<-c(list(list(cur_level, element)), back_path)
    # if (!inherits(cur_level, "list")) {
    #   stop(paste("no children, cannot proceed paste depth",depth,
    #     "to find element", element))
    # }
    cur_level<-cur_level[[element]]
    if (is.null(cur_level)) {
      stop(paste("cannot find element",element,"at depth",depth))
    }
    depth<-depth+1
  }

  if (is.function(edit_command)) {
    # arbitrary function(current_value, ...edit_value)
    # cur_level<-edit_command(cur_level, edit_value)
    #cur_level<-do.call(edit_command, c(list(cur_level), edit_value))
     cur_level<-edit_command(cur_level, edit_value, ...)
  } else if (edit_command=="<-" | edit_command=="=") {
    # assign
    cur_level<-edit_value
  } else if (edit_command=="-") {
    # delete
    cur_level<-NULL
  } else if (edit_command=="+" || edit_command=="+0") {
    # append or prepend (to list or vector)

    # current implementation does not coerc to list;
    # commented-out code is for coercion to list implementation
    # if (!inherits(cur_level, "list")) {
    #   cur_level<-list(cur_level)
    # }
    if (is.null(cur_level[[edit_newlevel]])) {
      if (edit_command=="+0") {
        cur_level<-
          #c(stats::setNames(list(edit_value), edit_newlevel), cur_level)
          c(stats::setNames(edit_value, edit_newlevel), cur_level)
      } else {
        cur_level<-
          #c(cur_level, stats::setNames(list(edit_value), edit_newlevel))
          c(cur_level, stats::setNames(edit_value, edit_newlevel))
      }
      if (length(edit_path)==0)
        cur_level<-metrdata(cur_level)
        # if no edit path, the above concat will create replace top-level
        # metrdata obj with a newly created list
        # convert back to metrdata obk
    } else {
      if (edit_command=="+0") {
        #cur_level[[edit_newlevel]]<- c(list(edit_value), cur_level[[edit_newlevel]])
        cur_level[[edit_newlevel]]<- c(edit_value, cur_level[[edit_newlevel]])
      } else {
        #cur_level[[edit_newlevel]]<- c(cur_level[[edit_newlevel]], list(edit_value))
        cur_level[[edit_newlevel]]<- c(cur_level[[edit_newlevel]], edit_value)
      }
    }
  } else if (edit_command=="str+" || edit_command=="str+0") {
    # append or prepend (to string)
    if (!is.character(cur_level)) {
      stop("final element in edit path must be a string for str operations")
    }
    if (edit_command=="str+0") {
      cur_level<- paste0(edit_value, cur_level)
    } else {
      cur_level<- paste0(cur_level, edit_value)
    }
  } else {
    stop(paste("invalid edit_command", edit_command))
  }

  for (tuple in back_path) {
    # not check for duplicated names -
    # not sure it is possible to add duplicated name in current implementation
    # if (!is.null(names(cur_level))) {
    #   if (any(duplicated(names(cur_level))))
    #     stop_or_warn(paste("duplicated names found:", names(cur_level), collapse=","))
    # }

    next_level<-tuple[[1]]
    element<-tuple[[2]]
    next_level[[element]]<-cur_level
    cur_level<-next_level
  }

  new_metrdata_obj<-cur_level
  if (identical(metrdata_obj, obj)) {
    new_metrdata_obj
  } else {
    set_metadata(obj, new_metrdata_obj, replace = TRUE)
  }
}


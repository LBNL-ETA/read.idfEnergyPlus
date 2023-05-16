#' @importFrom magrittr %>%
NULL
#' Check whether a line is empty or has an end-of-object marker
#'
#' @param line a line of the idf file
#' @return TRUE if the line doesn't contain a comma (indicate the line is in the
#'     middle of an idf object), or contains a end-of-object marker";"
empty.or.with.end.marker <- function(line) {
    (!stringr::str_detect(line, ",")) || (stringr::str_detect(line, ";.*!"))
}

#' Get objects line indices
#' @param lines lines read from idf file
#' @param object.type the type of the object, like, "Construction,",
#'     "Material,", etc. need to contain the "," as some object types are a
#'     prefix of others. The default value is an empty string
#' @return a vector of integers, marking the line numbers of the start of the object
#' @export
get.object.line.idx.by.type <- function(lines, object.type="") {
    line.idx.lst <- which(stringr::str_detect(lines, object.type))
    ## to guard against object.type=""
    line.idx.lst <- line.idx.lst[line.idx.lst > 0]
    ## check previous line being empty or has the end-object marker
    line.idx.lst[which(sapply(line.idx.lst - 1, function(x) {empty.or.with.end.marker(lines[[x]])}))]
}

#' Get objects starting line indices by name
#' fixme: this cannot guarantee this line is the name line
#' @param lines lines read from idf file
#' @param object.name name of the object. The default value is an empty string.
#'     The name can be a regex
#' @return a vector of integers, marking the line numbers of the start of the
#'     object (like the location of "Construction,")
#' @export
get.object.line.idx.by.name <- function(lines, object.name="") {
    line.idx.lst <- which(stringr::str_detect(lines, object.name)) - 1
    line.idx.lst <- line.idx.lst[line.idx.lst > 0]
    line.idx.lst[which(sapply(line.idx.lst - 1, function(x) {empty.or.with.end.marker(lines[[x]])}))]
}

#' Get objects starting line indices by type and or name
#' @param lines lines read from idf file
#' @param object.type the type of the object, like, "Construction,",
#'     "Material,", etc. need to contain the "," as some object types are a
#'     prefix of others. The default value is an empty string
#' @param object.name name of the object. The default value is an empty string
#'     The name can be a regex
#' @return a vector of integers, marking the line numbers of the start of the
#'     object (like the location of "Construction,")
#' @export
get.object.line.idx <- function(lines, object.type="", object.name="", verbose=FALSE) {
    line.idx.by.type = get.object.line.idx.by.type(lines, object.type)
    line.idx.by.name = get.object.line.idx.by.name(lines, object.name)
    if (verbose) {
        print(line.idx.by.type)
        print(line.idx.by.name)
    }
    intersect(line.idx.by.type, line.idx.by.name)
}

#' Get number of objects
#'
#' @param lines lines read from idf file
#' @param object.type the type of the object, like, "Construction,",
#'     "Material,", etc. need to contain the "," as some object types are a
#'     prefix of others
#' @return an integer of the number of object instances in the idf file
#' @export
get.object.count.by.type <- function(lines, object.type) {
    length(get.object.line.idx.by.type(lines, object.type))
}

#' Get names of objects for a certain type
#'
#' @param lines lines read from idf file
#' @param object.type the type of the object, like, "Construction,",
#'     "Material,", etc. need to contain the "," as some object types are a
#'     prefix of others
#' @return a vector of strings containing the object name lines for the
#'     specified object type
#' @export
get.object.names <- function(lines, object.type) {
    lines[get.object.line.idx.by.type(lines, object.type) + 1]
}

#' Get indices of idf object end
#'
#' @param lines lines read from idf file
#' @return a vector of integers, marking the line numbers of the end of all idd objects
#' @export
get.object.end.idx <- function(lines) {
    which(stringr::str_detect(lines, ";.*!"))
}

#' Get start and end line index of idf objects
#' @param lines lines read from idf file
#' @param object.type the type of the object, like, "Construction,",
#'     "Material,", etc. need to contain the "," as some object types are a
#'     prefix of others
#' @param object.name name of the object. The default value is an empty string
#'     The name can be a regex
#' @return two equal-length vectors marking the start and end of the objects
get.start.end.idx <- function(lines, object.type="", object.name="", verbose=FALSE) {
    end.of.all.objects <- get.object.end.idx(lines)
    start.of.objects <- get.object.line.idx(lines, object.type, object.name)
    print(sprintf("%d chunks found", length(start.of.objects)))
    if (verbose) {
        print(sprintf("start.of.objects: %s", paste(start.of.objects, collapse = " ")))
    }
    end.of.objects <- sapply(start.of.objects,
                             function(x) {end.of.all.objects[[which(x < end.of.all.objects)[[1]]]]})
    if (verbose) {
        print(sprintf("end.of.objects: %s", paste(end.of.objects, collapse = " ")))
    }
    assertthat::assert_that(length(start.of.objects) == length(end.of.objects))
    list(start.idx.lst = start.of.objects, end.idx.lst = end.of.objects)
}

#' Get the idf chunks given specified type or object name
#'
#' @param lines lines read from idf file
#' @param object.type the type of the object, like, "Construction,",
#'     "Material,", etc. need to contain the "," as some object types are a
#'     prefix of others
#' @param object.name name of the object. The default value is an empty string
#'     The name can be a regex
#' @return a vector of strings containing the chunks matching the specified
#'     object type and name
#' @export
get.idf.chunk <- function(lines, object.type="", object.name="", verbose=FALSE) {
    result = get.start.end.idx(lines, object.type, object.name, verbose)
    start.of.objects = result$start.idx.lst
    end.of.objects = result$end.idx.lst
    lapply(seq_along(start.of.objects), function(i) {
        lines[start.of.objects[[i]]:end.of.objects[[i]]]
    }) %>%
        unlist()
}

#' replace a chunk (object) in idf file
#'
#' This searches the start of the idf object by substring.pattern, and replace
#' this chunk with a new chunk
#'
#' @param lines lines read from idf file
#' @param new.text the text used to substitute the old chunk, starting from the
#'     object name line
#' @param object.type the type of the object, like, "Construction,",
#'     "Material,", etc. need to contain the "," as some object types are a
#'     prefix of others
#' @param object.name name of the object. it is guarenteed unique from
#'     EnergyPlus. If this is not specified and object.type have multiple
#'     instances, then only the first chunk is replaced
#' @param verbose if true, print debug outputs
#' @return a vector of lines with the chunk replaced
#' @export
replace.idf.chunk <- function(lines, new.text, object.type="", object.name="", verbose=FALSE) {
    result = get.start.end.idx(lines, object.type, object.name, verbose)
    location.obj.line.start = result$start.idx.lst
    location.obj.line.end = result$end.idx.lst
    print(sprintf("replace line %d to line %d", location.obj.line.start, location.obj.line.end))
    start.chunk = c(lines[1:(location.obj.line.start[[1]] - 1)], new.text)
    end.chunk = c(lines[(location.obj.line.end[[length(location.obj.line.end)]] + 1):length(lines)])
    if (length(location.obj.line.start) > 1) {
        middle.chunk = unlist(sapply(1:(length(location.obj.line.start) - 1), function(i) {
            c(lines[(location.obj.line.end[[i]] + 1):(location.obj.line.start[[i + 1]] - 1)], new.text)
        }))
        return(c(start.chunk, middle.chunk, end.chunk))
    } else {
        return(c(start.chunk, end.chunk))
    }
}


#' replace design day object in an input idf with a corresponding design day
#' object in a ddy file
#'
#' @param lines lines read from idf file
#' @param ddy.lines lines read from ddy file containing design days
#' @param design.day.kw design day object name or a substring (can be regex) to
#'     locate the design day object in the idd file and to match the
#'     corresponding one in the ddy file
#' @param verbose if true, print debug outputs
#' @return a vector of lines with the chunk replaced
#' @export
replace.design.day <- function(lines, ddy.lines, design.day.kw, verbose=FALSE) {
    design.day.start <- get.object.line.idx.by.name(lines, design.day.kw)
    design.day.setting <- stringr::str_extract(lines[design.day.start + 1],
                                               sprintf("%s.*\\,", design.day.kw))
    design.day = get.idf.chunk(ddy.lines, object.name = design.day.setting)
    read.idfEnergyPlus::replace.idf.chunk(lines, new.text=design.day,
                                          object.type = "SizingPeriod:DesignDay,",
                                          object.name= design.day.kw,
                                          verbose=verbose)
}

#' Get the line index of an object field
#'
#' @param lines lines read from idf file
#' @param object.type the type of the object, like, "Construction,",
#'     "Material,", etc. need to contain the "," as some object types are a
#'     prefix of others
#' @param object.name name of the object. it is guarenteed unique from
#'     EnergyPlus. If supply with empty string, then the name field is ignored
#' @param field.name name of the field whose value is to be retrieved
#' @param verbose if true, print debug outputs
#' @return a vector of integers marking the line number of the object field
#'     within the specified objects. The returned vector has the same length as
#'     the number of objects found with object.type-object.name
#' @export
get.field.idx <- function(lines, field.name, object.type="", object.name="", verbose=FALSE) {
    result = get.start.end.idx(lines, object.type, object.name, verbose)
    location.obj.line.start = result$start.idx.lst
    location.obj.line.end = result$end.idx.lst
    all.field.lines <- which(stringr::str_detect(lines, field.name))
    if (verbose) {
        print(sprintf("all field lines: %s", paste(all.field.lines, collapse = " ")))
    }
    sapply(seq_along(location.obj.line.start), function(i) {
        for (field.line in all.field.lines) {
            if ((location.obj.line.start[[i]] < field.line) && (field.line <= location.obj.line.end[[i]])) {
                return(field.line)
            }
        }
        print(sprintf("no %s line found within specified object", field.name))
        return(NULL)
    })
}

#' Get the value of an idf object field
#'
#' @param lines lines read from idf file
#' @param object.type the type of the object, like, "Construction,",
#'     "Material,", etc. need to contain the "," as some object types are a
#'     prefix of others
#' @param object.name name of the object. it is guarenteed unique from
#'     EnergyPlus. If supply with empty string, then the name field is ignored
#' @param field.name name of the field whose value is to be retrieved
#' @param verbose if true, print debug outputs
#' @return a string version of the value (might have prefix spaces)
#' @export
get.field.value <- function(lines, field.name, object.type="", object.name="", verbose=FALSE) {
    field.lines <- get.field.idx(lines, field.name, object.type, object.name, verbose)
    sapply(field.lines, function(field.line.idx) unlist(stringr::str_split(lines[field.line.idx], "[,;]"))[[1]])
}

#' get the value part from a line
#'
#' The field value ends with either a "," or a ";". This function retrieves the part before a "," or ";" for a given line
#'
#' @param field.lines a line whose value needs to be parsed
#' @return the portion before the first "," or a ";"
get.field.value.from.one.line <- function(field.line) {
    delim.idx <- unlist(gregexpr("[,;]", field.line))
    substr(field.line, 1, delim.idx - 1)
}

#' replace the value of a field in idf file
#'
#' replace value of a field in idf file
#'
#' @param lines lines read from idf file
#' @param new.value the text used to substitute the old value
#' @param object.type the type of the object, like, "Construction,",
#'     "Material,", etc. need to contain the "," as some object types are a
#'     prefix of others
#' @param object.name name of the object. it is guarenteed unique from
#'     EnergyPlus. If this is not specified and object.type have multiple
#'     instances, then only the first chunk is replaced
#' @param field.name name of the field whose value is to be retrieved
#' @param fun if the new value is a function of the old value, pass the function
#'     here. To replace it with a specific value, pass in a constant function.
#'     The function input is a string
#' @param verbose if true, print debug outputs
#' @return a vector of lines with the value replaced
#' @export
replace.field.value <- function(lines, object.type, object.name, field.name, fun, verbose=FALSE) {
    field.line.idx.lst <- get.field.idx(lines, field.name, object.type, object.name, verbose)
    for (field.line.idx in field.line.idx.lst) {
        field.line = lines[field.line.idx]
        delim.idx <- unlist(gregexpr("[,;]", field.line))
        value = substr(field.line, 1, delim.idx - 1)
        lines[field.line.idx] = paste0(as.character(fun(value)), substr(field.line, delim.idx, nchar(field.line)))
        print(sprintf("replaced line %d", field.line.idx))
        print(sprintf("before: %s", field.line))
        print(sprintf("after: %s", lines[field.line.idx]))
    }
    return(lines)
}

#' Apply get function to a list of files
#'
#' Aim to retrieve some statistics or field value for a group of idf files (in
#' the same folder) what stats/value to get depends on "fun" argument
#'
#' @param files the list of files to get value from
#' @param path the folder containing the list of files
#' @param fun function to apply to each file to retrieve the desired value,
#'     examples include get.object.count.by.type, get.object.names,
#'     get.field.value. The arguments of fun is in the "..."
#' @return a dataframe with two columns, filename, and value
#' @export
apply.fun.to.files <- function(files, path, fun, ...) {
    lapply(files, function(f) {
        lines = readLines(sprintf("%s/%s", path, f))
        tibble::tibble(filename = f, value = fun(lines, ...))
    }) %>%
        dplyr::bind_rows() %>%
        {.}
}

#' Read a schedule object into a data frame
#'
#' @param schedule.type first line of a schedule object, like "Schedule:Day:Interval,"
#' @param schedule.name object name, like "single_family_first_story heating week 0 Sunday,"
#' @return a dataframe with two columns with string type, "time" and "value". The "time" column contains the value of the schedule.
read.schedule <- function(lines, schedule.type, schedule.name, verbose=FALSE) {
    result = get.start.end.idx(lines, object.type=schedule.type, object.name=schedule.name, verbose)
    location.obj.line.start = result$start.idx.lst
    location.obj.line.end = result$end.idx.lst
    object.lines = lines[location.obj.line.start:location.obj.line.end]
    print(sprintf("found schedule line %d to line %d", location.obj.line.start, location.obj.line.end))
    schedule.time.lines.idx = which(sapply(object.lines, function(x) {any(stringr::str_detect(x, sprintf("%02d:00", 1:24)))}))
    if(verbose) {
        print(sprintf("schedule time lines: %s", paste(schedule.time.lines.idx, collapse = " ")))
    }
    schedule.value.lines.idx = schedule.time.lines.idx + 1
    schedule.time = sapply(object.lines[schedule.time.lines.idx], get.field.value.from.one.line)
    schedule.value = sapply(object.lines[schedule.value.lines.idx], get.field.value.from.one.line)
    tibble::tibble(time=schedule.time, value=schedule.value)
}


#' Extract the people linked to a publication. This can be the authors or the editors.
#'
#' When an xml file representing publications is given, the function extracts the authors (and editors) into an R list.
#'
#'
#' @param xmlNode A xml node representing one article
#' @param strCategory A string indicating which sub-elements should be used
#' @return A data.frame containing sorted by the indicated ordering, containing the first middle and last name for each person
#' @export
#' @author Klaus Herrmann
personsDataFrame <- function(xmlNode,strCategory=".//author"){
    require(xml2)
    xmlAuthors <- xml2::xml_find_all(xmlNode, strCategory)
    nrAuthors <- length(xmlAuthors)
    authors <- data.frame(matrix(NA, nrow = nrAuthors, ncol = 4))
    colnames(authors) <- c("order","first","middle","last")
    for (k in 1:nrAuthors) {
        author <- xmlAuthors[k]
        first <- xml2::xml_attr(author,"first")
        middle <- xml2::xml_attr(author,"middle")
        last <- xml2::xml_attr(author,"last")
        order <- xml2::xml_attr(author,"order")
        authors[k,] <- c(order, first, middle, last)
    }
    authors <- authors[order(authors$order, decreasing = FALSE), ] 
}


#' Function that transforms First Middle and Last name into a standard citation style string
#'
#' The function transforms names into standard citation style strings. For example Rainer Maria Rilke is converted to Rilke, R.M.
#' The function breaks at spaces or hyphen, see the examples.
#'
#' @param first First name of the person
#' @param middle Middle name of the person (or NA)
#' @param last Last name of the person
#' @return A string with the last name followed by the extracted initials
#' @examples
#' print(citeName("Rainer","Maria","Rilke")) #Rilke, R.M.
#' print(citeName("Jean-Luc", "Borg", "Picard")) #Picard, J.L.B.
#' @export
#' @author Klaus Herrmann
citeName <- function(first,middle,last){
    if (is.na(middle)){middle<-""} #in case middle is not given in the xml file
    splitString <- "\\,\\s|\\-|\\s" #regex that breaks at every space or dash
    splitFirst <- unlist(strsplit(first,splitString))
    splitMiddle <- unlist(strsplit(middle,splitString))
    names <- c(splitFirst, splitMiddle)
    initials <- paste(substr(names[1],1,1),".",sep="")
    if (length(names)>1){
        for (k in 2:length(names)) {
            initials <- paste(initials,substr(names[k],1,1),".",sep="")
        }
    }
    out <- paste(last,", ", initials, sep="")
}


#' Create an R list object from a xml node representing an in collection publication
#'
#' The function transforms a xml node representing an in collection publication into a list.
#'
#'
#' @param xmlNode The xml node with the information
#' @param dateFormat A string specifying how the dates in the xml node are represented
#' @return A list containing the information extracted from the xml node
#' @export
#' @author Klaus Herrmann
xml2lstInCollection <- function(xmlNode,dateFormat="%Y-%m-%d"){
    require(xml2)
    type <- xml2::xml_attr(xmlNode,"type")
    status <- xml2::xml_attr(xmlNode,"status")
    date <- xml2::xml_attr(xmlNode,"date")
    date <- as.Date(date,format=dateFormat)
    title <- xml2::xml_attr(xmlNode,"title")
    #journal <- xml2::xml_attr(xmlArticle,"journal")
    bookTitle <- xml2::xml_attr(xmlNode,"booktitle")
    #issue <- xml2::xml_attr(article,"issue")
    volume <- xml2::xml_attr(xmlNode,"volume")
    #number <- xml2::xml_attr(xmlNode,"number")
    pageFirst <- xml2::xml_attr(xmlNode,"pageFirst")
    pageLast <- xml2::xml_attr(xmlNode,"pageLast")
    publisher <- xml2::xml_attr(xmlNode,"publisher")
    refereed <- xml2::xml_attr(xmlNode,"refereed")
    openAccess <- xml2::xml_attr(xmlNode,"openAccess")
    series <- xml2::xml_attr(xmlNode,"series")
    authors <- personsDataFrame(xmlNode,".//author")
    editors <- personsDataFrame(xmlNode,".//editor")
    data <- list(type=type, status=status, date=date, title=title, bookTitle=bookTitle, volume=volume, pageFirst=pageFirst, pageLast=pageLast, publisher=publisher, refereed=refereed, openAccess=openAccess, series=series, authors=authors, editors=editors)
}


#' Convert in collection list to a markdown string
#'
#' The function transforms a list representing an in collection publication into a markdown string.
#'
#'
#' @param lstInCollection The list with the information
#' @return A string in the markdown format with a standard citation for the article
#' @export
#' @author Klaus Herrmann
formatInCollectionMD <- function(lstInCollection){
    authors <- lstInCollection$authors
    nrAuthors <- nrow(authors)
    strAuthors <- citeName(authors[1,2],authors[1,3],authors[1,4])
    if (nrAuthors > 1){ 
        for (k in 2:nrAuthors) {
            newName = citeName(authors[k,2],authors[k,3],authors[k,4])
            strAuthors <- paste(strAuthors, ", ", newName,sep="")
        }
    }

    editors <- lstInCollection$editors
    nrEditors <- nrow(editors)
    strEditors <- citeName(editors[1,2],editors[1,3],editors[1,4])
    if (nrEditors > 1){ 
        for (k in 2:nrEditors) {
            newName = citeName(editors[k,2],editors[k,3],editors[k,4])
            strEditors <- paste(strEditors, ", ", newName,sep="")
        }
        strEditors <- paste(strEditors, " (eds)", sep="")
    } else
    {
        strEditors <- paste(strEditors, " (ed)", sep="")
    }

    year <- format(lstInCollection$date, format="%Y")
	outString <- paste(strAuthors," (",year,"). **",lstInCollection$title,".** In: ",strEditors," *",lstInCollection$bookTitle,"*. ", sep="")


    if (lstInCollection$series != ""){
		outString <- paste(outString,lstInCollection$series,sep="")
	}

    if (lstInCollection$volume != ""){
		outString <- paste(outString,", vol ",lstInCollection$volume, ", ", sep="")
	}

    outString <- paste(outString,lstInCollection$publisher,", ",lstInCollection$pageFirst,"--",lstInCollection$pageLast,".",sep="")
}


#' Create an R list object from a xml node representing a journal article
#'
#' The function transforms a xml node representing an journal article into a list.
#'
#'
#' @param xmlNode The xml node with the information
#' @param dateFormat A string specifying how the dates in the xml node are represented
#' @return A list containing the information extracted from the xml node
#' @export
#' @author Klaus Herrmann
xml2lstJournalArticle <- function(xmlNode,dateFormat="%Y-%m-%d"){
    require(xml2)
    type <- xml2::xml_attr(xmlNode,"type")
    status <- xml2::xml_attr(xmlNode,"status")
    #year <- xml2::xml_attr(article,"year")
    date <- xml2::xml_attr(xmlNode,"date")
    date <- as.Date(date,format=dateFormat)
    title <- xml2::xml_attr(xmlNode,"title")
    journal <- xml2::xml_attr(xmlNode,"journal")
    #issue <- xml2::xml_attr(article,"issue")
    volume <- xml2::xml_attr(xmlNode,"volume")
    number <- xml2::xml_attr(xmlNode,"number")
    pageFirst <- xml2::xml_attr(xmlNode,"pageFirst")
    pageLast <- xml2::xml_attr(xmlNode,"pageLast")
    publisher <- xml2::xml_attr(xmlNode,"publisher")
    refereed <- xml2::xml_attr(xmlNode,"refereed")
    openAccess <- xml2::xml_attr(xmlNode,"openAccess")

    authors <- personsDataFrame(xmlNode,".//author")
    #editors <- personsDataFrame(article,".//editor")
    data <- list(type=type, status=status, date=date, title=title, journal=journal, volume=volume, number=number, pageFirst=pageFirst, pageLast=pageLast, publisher=publisher, refereed=refereed, openAccess=openAccess, authors=authors)

}


#' Convert journal article list to a markdown string
#'
#' The function transforms a list representing a journal article into a markdown string.
#'
#'
#' @param lstArticle The list with the information
#' @return A string in the markdown format with a standard citation for the article
#' @export
#' @author Klaus Herrmann
formatJournalArticleMD <- function(lstArticle){
    authors <- lstArticle$authors
    nrAuthors <- nrow(authors)
    strAuthors <- citeName(authors[1,2],authors[1,3],authors[1,4])
    if (nrAuthors > 1){ 
        for (k in 2:nrAuthors) {
            newName = citeName(authors[k,2],authors[k,3],authors[k,4])
            strAuthors <- paste(strAuthors, ", ", newName,sep="")
        }
    }
    year <- format(lstArticle$date, format="%Y")
	outString <- paste(strAuthors," (",year,"). **",lstArticle$title,".** *",lstArticle$journal,"*, ", lstArticle$volume,sep="")
    #outString <- paste("**",lstArticle$title,"**.  \n",strAuthors," (",year,"). *",lstArticle$journal,"*, ", lstArticle$volume,sep="")



    if (lstArticle$number != ""){
		outString <- paste(outString,"(",lstArticle$number,")",sep="")
	}
    outString <- paste(outString,", ",lstArticle$pageFirst,"--",lstArticle$pageLast,".",sep="")
}


#' Create an R list object from a xml node representing a publication of type other
#'
#' The function transforms a xml node representing an unspecified (other) publication into a list.
#'
#'
#' @param xmlNode The xml node with the information
#' @param dateFormat A string specifying how the dates in the xml node are represented
#' @return A list containing the information extracted from the xml node
#' @export
#' @author Klaus Herrmann
xml2lstOther <- function(xmlNode,dateFormat="%Y-%m-%d"){
    require(xml2)
    type <- xml2::xml_attr(xmlNode,"type")
    status <- xml2::xml_attr(xmlNode,"status")
    date <- xml2::xml_attr(xmlNode,"date")
    date <- as.Date(date,format=dateFormat)
    title <- xml2::xml_attr(xmlNode,"title")
    journal <- xml2::xml_attr(xmlNode,"journal")
    #bookTitle <- xml2::xml_attr(xmlNode,"booktitle")
    #issue <- xml2::xml_attr(article,"issue")
    volume <- xml2::xml_attr(xmlNode,"volume")
    number <- xml2::xml_attr(xmlNode,"number")
    pageFirst <- xml2::xml_attr(xmlNode,"pageFirst")
    pageLast <- xml2::xml_attr(xmlNode,"pageLast")
    publisher <- xml2::xml_attr(xmlNode,"publisher")
    refereed <- xml2::xml_attr(xmlNode,"refereed")
    openAccess <- xml2::xml_attr(xmlNode,"openAccess")
    #series <- xml2::xml_attr(xmlNode,"series")
    authors <- personsDataFrame(xmlNode,".//author")
    #editors <- personsDataFrame(xmlNode,".//editor")
    data <- list(type=type, status=status, date=date, title=title, journal=journal, volume=volume, number=number, pageFirst=pageFirst, pageLast=pageLast, publisher=publisher, refereed=refereed, openAccess=openAccess, authors=authors)

}


#' Convert other publication list to a markdown string
#'
#' The function transforms a list representing a other publication into a markdown string.
#'
#'
#' @param lstArticle The list with the information
#' @return A string in the markdown format with a standard citation for the article (other)
#' @export
#' @author Klaus Herrmann
formatOtherMD <- function(lstOther){
    authors <- lstOther$authors
    nrAuthors <- nrow(authors)
    strAuthors <- citeName(authors[1,2],authors[1,3],authors[1,4])
    if (nrAuthors > 1){ 
        for (k in 2:nrAuthors) {
            newName = citeName(authors[k,2],authors[k,3],authors[k,4])
            strAuthors <- paste(strAuthors, ", ", newName,sep="")
        }
    }

    year <- format(lstOther$date, format="%Y")
	outString <- paste(strAuthors," (",year,"). **",lstOther$title,".** *",lstOther$journal,"*", sep="")

    if (lstOther$volume != ""){
		outString <- paste(outString,", vol ",lstOther$volume, sep="")
	}

    if (lstOther$number != ""){
		outString <- paste(outString,"(",lstOther$number, ")", sep="")
	}

    outString <- paste(outString,". ",lstOther$publisher,", ",lstOther$pageFirst,"--",lstOther$pageLast,".",sep="")
}


#' Extract the information linked to a conference venue.
#'
#' The function extracts the event, location and miscellaneous information from a xml node linked to a poster or a talk.
#' This gives the event name, the location (it is necessary to have a sub-node in each language: Brussels vs. Bruxelles) and miscellaneous information in a list.
#'
#'
#' @param xmlNode The xml node with the information
#' @param strLanguage A string specifying which language should be used
#' @return A list containing the information (event, location, misc) extracted from the xml node
#' @export
#' @author Klaus Herrmann
conferenceDetails <- function(xmlNode,strLanguage=".//eng"){
    require(xml2)
    details <- xml2::xml_find_all(xmlNode, strLanguage)
    nrDetails <- length(xmlNode)

    if (nrDetails != 1 ){
        stop()
    }

    event <- xml2::xml_attr(details,"event")
    location <- xml2::xml_attr(details,"location")
    misc <- xml2::xml_attr(details,"misc")

    return(list(event=event, location=location, misc=misc))
}


#' Create an R list object from a xml Poster node
#'
#' The function transforms a xml node representing a poster presentation into a list.
#' 
#'
#' @param xmlPoster The xml node with the information
#' @param dateFormat A string specifying how the dates in the xml node are represented
#' @param strLanguage A string specifying which language should be used
#' @return A list containing the information extracted from the xml node
#' @export
#' @author Klaus Herrmann
xml2lstPoster <- function(xmlPoster,dateFormat="%Y/%m/%d", strLanguage=".//eng"){
    require(xml2)
    type <- xml2::xml_attr(xmlPoster,"type")
    title <- xml2::xml_attr(xmlPoster,"title")
    
    date <- xml2::xml_attr(xmlPoster,"date")
    date <- as.Date(date,format=dateFormat)

    start <- xml2::xml_attr(xmlPoster,"start")
    start <- as.Date(start,format=dateFormat)

    stop <- xml2::xml_attr(xmlPoster,"stop")
    stop <- as.Date(stop,format=dateFormat)

    details <- conferenceDetails(xmlPoster,strLanguage)

    data <- list(type=type, date=date, title=title, start=start, stop=stop, event=details$event, location=details$location, misc=details$misc)
}


#' Convert poster list to a markdown string
#'
#' The function transforms a list representing a poster into a markdown string.
#' The second parameter allows to change the language used for the months (April vs. avril).
#' Possible values are c(LC_ALL","en_US.UTF-8") or c(LC_ALL","fr_FR.UTF-8"). German is somehow not working.
#'
#'
#' @param lstPoster The list with the information
#' @param strSetlocale The list with the information
#' @return A string in the markdown format with a standard citation for the poster
#' @export
#' @author Klaus Herrmann
formatPosterMD <- function(lstPoster, strSetlocale=c("LC_ALL","en_US.UTF-8"),miscCol="darkred"){
    Sys.setlocale(strSetlocale[1],strSetlocale[2])
    outString <- paste("**",lstPoster$title,"**",sep="")

    if (lstPoster$type == "invited"){
        outString <- paste(outString," (*invited poster*). ",sep="")
    } else {
        outString <- paste(outString,". ",sep="")
    }

    outString <- paste(outString, lstPoster$event, sep="")

    if (is.na(lstPoster$date)==FALSE){
           outString <- paste(outString, ", ",format(lstPoster$date, format="%d %B %Y"),sep="")
    }
  
    if (is.na(lstPoster$location)==FALSE){
        if (lstPoster$location != ""){
            outString <- paste(outString, ", ", lstPoster$location,".",sep="")
        } else {
            outString <- paste(outString, ".",sep="")
        } 
    } else {
        outString <- paste(outString, ".",sep="") 
    }

    if (is.na(lstPoster$misc)==FALSE){
        if (lstPoster$misc != ""){
            outString <- paste(outString," <span style=\"color:",miscCol,"\">*",lstPoster$misc,"*.</span>",sep="")
        }
    }

    return(outString)
}


#' Create an R list object from a xml Presentation (Talk) node
#'
#' The function transforms a xml node representing a talk into a list.
#' 
#'
#' @param xmlPoster The xml node with the information
#' @param dateFormat A string specifying how the dates in the xml node are represented
#' @param strLanguage A string specifying which language should be used
#' @return A list containing the information extracted from the xml node
#' @export
#' @author Klaus Herrmann
xml2lstPresentation <- function(xmlPresentation,dateFormat="%Y/%m/%d", strLanguage=".//eng"){
    require(xml2)

    type <- xml2::xml_attr(xmlPresentation,"type")
    title <- xml2::xml_attr(xmlPresentation,"title")
    
    date <- xml2::xml_attr(xmlPresentation,"date")
    date <- as.Date(date,format=dateFormat)

    start <- xml2::xml_attr(xmlPresentation,"start")
    start <- as.Date(start,format=dateFormat)

    stop <- xml2::xml_attr(xmlPresentation,"stop")
    stop <- as.Date(stop,format=dateFormat)

    details <- conferenceDetails(xmlPresentation,strLanguage)

    data <- list(type=type, date=date, title=title, start=start, stop=stop, event=details$event, location=details$location, misc=details$misc)
}


#' Convert presentation (talk) list to a markdown string
#'
#' The function transforms a list representing a presentation (talk) into a markdown string.
#' The second parameter allows to change the language used for the months (April vs. avril).
#' Possible values are c(LC_ALL","en_US.UTF-8") or c(LC_ALL","fr_FR.UTF-8"). German is somehow not working.
#'
#'
#' @param lstPresentation The list with the information
#' @param strSetlocale The list with the information
#' @return A string in the markdown format with a standard citation for the talk
#' @export
#' @author Klaus Herrmann
formatPresentationMD <- function(lstPresentation, strSetlocale=c("LC_ALL","en_US.UTF-8"),miscCol="darkred"){
    Sys.setlocale(strSetlocale[1],strSetlocale[2])
    outString <- paste("**",lstPresentation$title,"**",sep="")

    if (lstPresentation$type == "invited"){
        outString <- paste(outString," (*invited talk*). ",sep="")
    } else {
        outString <- paste(outString,". ",sep="")
    }

    outString <- paste(outString, lstPresentation$event, sep="")

    if (is.na(lstPresentation$date)==FALSE){
           outString <- paste(outString, ", ",format(lstPresentation$date, format="%d %B %Y"),sep="")
    }
  
    if (is.na(lstPresentation$location)==FALSE){
        if (lstPresentation$location != ""){
            outString <- paste(outString, ", ", lstPresentation$location,".",sep="")
        } else {
            outString <- paste(outString, ".",sep="")
        } 
    } else {
        outString <- paste(outString, ".",sep="") 
    }

    if (is.na(lstPresentation$misc)==FALSE){
        if (lstPresentation$misc != ""){
            outString <- paste(outString," <span style=\"color:",miscCol,"\">*",lstPresentation$misc,"*.</span>",sep="")
        }
    }

    return(outString)
}


#' Create an R list object from a xml node representing a course (teaching)
#'
#' The function transforms a xml node representing a course into a list.
#' 
#'
#' @param xmlNode The xml node with the information
#' @param dateFormat A string specifying how the dates in the xml node are represented
#' @return A list containing the information extracted from the xml node
#' @export
#' @author Klaus Herrmann
xml2lstTeaching <- function(xmlNode,dateFormat="%Y-%m-%d"){
    require(xml2)
    role <- xml2::xml_attr(xmlNode,"role")
    
    begin <- xml2::xml_attr(xmlNode,"begin")
    begin <- as.Date(begin,format=dateFormat)

    end <- xml2::xml_attr(xmlNode,"end")
    end <- as.Date(end,format=dateFormat)


    level <- xml2::xml_attr(xmlNode,"level")
    institution <- xml2::xml_attr(xmlNode,"institution")
    nrstudents <- xml2::xml_attr(xmlNode,"nrstudents")

    title <- xml2::xml_text(xmlNode)

    data <- list(role=role, begin=begin, end=end, level=level, institution=institution, nrstudents=nrstudents, title=title)
}


#' Convert course (teaching) list to a markdown string
#'
#' The function transforms a list representing a course (teaching) into a markdown string.
#'
#'
#' @param lstCours The list with the information
#' @return A string in the markdown format with a representation of the cours
#' @export
#' @author Klaus Herrmann
formatTeachingMD <- function(lstCours){
    begin <- lstCours$begin
    end <- lstCours$end
    level <- lstCours$level
    institution <- lstCours$institution
    title <- lstCours$title

    if (format(begin, format="%Y") == format(end, format="%Y")){
        strOut <- paste(format(begin, format="%Y"),": ",sep="")
    }else{
        strOut <- paste(format(begin, format="%Y"),"/",format(end, format="%Y"),": ",sep="")
    }

    strOut <- paste(strOut,"**",title,"**",sep="")

    if (level == "undergraduate"){
        strLevel <- "Undergraduate level"
    }

    if (level == "masters"){
        strLevel <- "Masters level"
    }

    strOut <- paste(strOut,", ",strLevel,", ",institution,".",sep="")
}


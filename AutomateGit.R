#==============================================================================#
#                                                                              #
#    Title      : Automate Git                                                 #
#    Purpose    : Define function for automating Git processes.                #
#    Notes      : .                                                            #
#    Author     : chrimaho                                                     #
#    Created    : 09/May/2020                                                  #
#    References : .                                                            #
#    Sources    : .                                                            #
#    Edited     : 09/May/2020 - Initial creation                               #
#                                                                              #
#==============================================================================#

# Start ----
GitSync <- function(repo=rprojroot::find_rstudio_root_file(), untracked=TRUE, stage=TRUE, commit=TRUE, pull=TRUE, push=TRUE) {
    #' @title Sync Git
    #' @description Automate the Sync process for Git.
    #' @note This will run through the Git stages in sequence: `untracked`, `stage`, `commit`, `pull`, `push`.
    #' @param repo character. The working directory to be Sync'd. Must be a valid system path.
    #' @param untracked logical. Do you want to run the `untracked` process?
    #' @param stage logical. Do you want to run the `stage` process?
    #' @param commit logical. Do you want to run the `commit` process?
    #' @param pull logical. Do you want to run the `pull` process?
    #' @param push logical. Do you want to run the `push` process?
    #' @return A character string for success or failure.
    
    # Confirm required packages are loaded ----
    require(git2r)
    require(rprojroot)
    require(rstudioapi)
    require(assertthat)
    
    # Validations ----
    assert_that(is.character(repo))
    assert_that(is.logical(untracked))
    assert_that(is.logical(stage))
    assert_that(is.logical(commit))
    assert_that(is.logical(pull))
    assert_that(is.logical(push))
    assert_that(dir.exists(repo), msg="'repo' must be a valid system directory.")
    
    # Confirm valid repor directory ----
    if (!file.exists(paste0(repo, "/.git/config"))) {
        stop(paste0("You have not supplied a valid repo directory. '", repo, "'"))
    }
    
    # Get credentials from user input ----
    get_Credentials <- function() {
        
        # Get & Set username
        if (is.null(getOption("git_username"))) {
            username <- showPrompt(title="Username", message="Enter your username:", default="")
        } else {
            username <- getOption("git_username")
        }
        
        # Get & Set password
        if (is.null(getOption("git_password"))) {
            password <- askForPassword(prompt="Enter your password:")
        } else {
            password <- getOption("git_password")
        }
        
        # Save username
        if (is.null(getOption("git_username"))) {
            confirm <- showQuestion(title="Save username?"
                                   ,message=paste0("Would you like to save your Git username to the `options()` environment?","\n\n"
                                                  ,"If 'Yes', then you will not be prompted for your username next time.","\n"
                                                  ,"If 'No', then you will be prompted for your username again next time.","\n\n"
                                                  ,"Note: `options()` are not pushed to Git.","\n"
                                                  ,"However, they are still retrievable by others on your computer.","\n"
                                                  ,"So be careful."
                                                  )
                                   ,ok="Yes"
                                   ,cancel="No"
                                   )
            if (confirm == TRUE) {
                options(git_username=username)
            }
        } else if (username != getOption("git_username")) {
            confirm <- showQuestion(title="Update username?"
                                   ,message=paste0("The username you have provided is different to what is saved.","\n"
                                                  ,"Would you like to update this new username to memory?"
                                                  )
                                   ,ok="Yes"
                                   ,cancel="No"
                                   )
            if (confirm == TRUE) {
                options(git_username=username)
            }
        }
        
        # Save password
        if (is.null(getOption("git_password"))) {
            confirm <- showQuestion(title="Save password?"
                                   ,message=paste0("Would you like to save your Git password to the `options()` environment?","\n\n"
                                                  ,"If 'Yes', then you will not be prompted for your password next time.","\n"
                                                  ,"If 'No', then you will be prompted for your password again next time.","\n\n"
                                                  ,"Note: `options()` are not pushed to Git.","\n"
                                                  ,"However, they are still retrievable by others on your computer.","\n"
                                                  ,"So be careful."
                                                  )
                                   ,ok="Yes"
                                   ,cancel="No"
                                   )
            if (confirm == TRUE) {
                options(git_password=password)
            }
        } else if (password != getOption("git_password")) {
            confirm <- showQuestion(title="Update password?"
                                   ,message=paste0("The password you have provided is different to what is saved.","\n"
                                                  ,"Would you like to update this new password to memory?"
                                                  )
                                   ,ok="Yes"
                                   ,cancel="No"
                                   )
            if (confirm == TRUE) {
                options(git_password=password)
            }
        }
        
        # Set Credentials
        credentials <- cred_user_pass(username=username, password=password)
        
        # Return
        return(credentials)
    }
    
    # NOTE: values returned from the status() command are as follows: ----
    # 1. "untracked" means new files which have not yet been added to GitHub.
    # 2. "unstaged" means existing files which have been modified but not yet ready to be committed to GitHub.
    # 3. "staged" means files that are staged and ready to be committed.
    
    # Process untracked ----
    if (untracked == TRUE) {
        num <- length(unlist(status()["untracked"]))
        if (num > 0) {
            writeLines(paste0("There are ", num, " Untracked items to be processed."))
            for (i in 1:num) {
                writeLines(paste0("    ", i, ": ",unlist(status()["untracked"])[i]))
            }
            git2r::add(repo, unlist(status()["untracked"]))
            writeLines(paste0("Items have been Staged."))
            CommitComment <- showPrompt(title="Git Commit Comment", message="Enter a comment for the commit message.")
            if (!nchar(trimws(CommitComment), keepNA = TRUE) %in% c("NA","0",NA,0)) {
                CommitComment <- paste(Sys.time(), CommitComment, sep = " - ")
            } else {
                CommitComment <- paste(Sys.time(), "Initial commit", sep = " - ")
            }
            commit(message = CommitComment)
            writeLines(paste0("Items have been Committed."))
            push(credentials = get_Credentials())
            writeLines(paste0("Items have been Pushed."))
        }
    }
    
    # Process stage ----
    if (stage == TRUE) {
        num <- length(unlist(status()["unstaged"]))
        if (num > 0) {
            writeLines(paste0("There are ", num, " Tracked items to be processed."))
            for (i in 1:num) {
                writeLines(paste0("    ", i, ": ", unlist(status()["unstaged"])[i]))
            }
        }
        if (!is.null(unlist(status()["unstaged"]))) {
            git2r::add(repo, unlist(status()["unstaged"]))
            num2 <- length(unlist(status()["unstaged"]))
            if (num2 == 0) {
                writeLines(paste0("Items have been Staged."))
            } else if (num == num2) {
                stop ("Something went wrong with the Staging.")
            }
        }
    }
    
    # Process commit ----
    if (commit == TRUE) {
        if (!is.null(unlist(status()["staged"]))) {
            CommitComment <- showPrompt(title="Git Commit Comment", message="Enter a comment for the commit message.")
            if (!nchar(trimws(CommitComment), keepNA = TRUE) %in% c("NA","0",NA,0)) {
                CommitComment <- paste(Sys.time(), CommitComment, sep = " - ")
            } else {
                CommitComment <- paste(Sys.time(), "Update", sep = " - ")
            }
            commit(message = CommitComment)
            num2 <- length(unlist(status()["staged"]))
            if (num2 == 0) {
                writeLines(paste0("Items have been Committed."))
            } else if (num == num2) {
                stop ("Something went wrong with Committing.")
            }
        }
    }
    
    # Process pull ----
    # tryCatch() is utilised because the error message when executing pull() or push() is not very helpful: "too many redirects or authentication replays". The main issue is usually that the credentials are incorrect or missing.
    if (pull == TRUE) {
        pull <- tryCatch ( 
            expr = {
                git2r::pull(credentials = get_Credentials())
            },
            error = function (err) {
                message (paste0("Error when Pulling from GitHub. Try checking your credentials and try again.","\n","Message thrown: "))
                stop (err)
            },
            warning = function (war) {
                message ("There was a Warning when Pulling from GitHub.")
                return (war)
            },
            finally = {
                # It was successful. Move on.
            }
        )
        if (unlist(pull["up_to_date"]) == TRUE) {
            writeLines(paste0("There are no discrepancies with the Master branch."))
        } else {
            stop ("Something went wrong with pulling the repo. Please manually check, merge the code, validate discrepancies, then re-try.")
        }
    }
    
    # Process push ----
    if (push == TRUE) {
        if (num > 0) {
            tryCatch(
                expr = {
                    push(credentials = get_Credentials())
                },
                error = function(err) {
                    message (paste0("Error when Pushing to GitHub. Try checking your credentials and try again.","\n","Message thrown: "))
                    stop (err)
                },
                warning = function (war) {
                    message ("There was a Warning when Pushing to GitHub.")
                    return (war)
                },
                finally = {
                    # It was successful. Move on.
                }
            )
            num2 <- length(unlist(status()))
            if (num2 == 0) {
                writeLines(paste0("Items have been Pushed."))
            } else if (num == num2) {
                stop ("Something went wrong with Pushing.")
            }
        }
    }
    
    # Return ----
    return(writeLines(paste0("Successfully updated.")))
}

GitSync()

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
require(dplyr)
library(shiny)
require(googlesheets)

select <- dplyr::select

update_dt <- "2018-04-30"


mkcls <- function(cond, old_ds, col_prefix){
  new_ds <- data.frame(n=1)
  if(sum(cond) > 0){
    if(sum(cond) > 1){
      totalmins30 <- sum(30 * as.numeric(old_ds$Freq[cond]) * as.numeric(old_ds$Duration[cond]) / as.numeric(old_ds$DaysPerCycle[cond]))
      freq <- new_ds[paste0(col_prefix, "_freq")] <- totalmins30/30
      dur <- new_ds[paste0(col_prefix, "_dur")] <- 30
      days <- new_ds[paste0(col_prefix, "_days")] <- 30
      new_ds[col_prefix] = sprintf("%g entries: %s", sum(cond), paste(sprintf("%gx%g per %g days", as.numeric(old_ds$Freq[cond]), as.numeric(old_ds$Duration[cond]), as.numeric(old_ds$DaysPerCycle[cond])), collapse=", "))
    } else {
      freq <- new_ds[paste0(col_prefix, "_freq")] <- as.numeric(old_ds$Freq[cond])
      dur <- new_ds[paste0(col_prefix, "_dur")] <- as.numeric(old_ds$Duration[cond])
      days <- new_ds[paste0(col_prefix, "_days")] <- as.numeric(old_ds$DaysPerCycle[cond])
      new_ds[col_prefix] = sprintf("%gx%g per %g days", freq, dur, days)
    }
  } else {
    new_ds[paste0(col_prefix, c("_freq", "_dur", "_days"))]  <- NA
  }
  new_ds
}

mkrw <- function(ds){
  ds1 <- mkcls(ds$Mode=="Consultation", ds, "consult")
  ds2 <- mkcls(ds$Mode=="SpecialEd - General", ds, "push_in")
  ds3 <- mkcls(ds$Mode=="SpecialEd - Other" & ds$Type == "Speech Lang 1.1", ds, "push_out_11")
  ds4 <- mkcls(ds$Mode=="SpecialEd - Other" & ds$Type != "Speech Lang 1.1", ds, "push_out_grp")
  ds_out <- cbind(ds1 %>% select(-n), ds2 %>% select(-n), ds3 %>% select(-n), ds4 %>% select(-n))
  ds_out
}


# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
  titlePanel("Uploading Files"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose CSV File',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      textInput("gs", label="Google Sheet extension", value="1Es_RC8SRopbvojrXxk_PRxyuQknG-4y9NRoY5ZmLr-s"),
      textInput("sht", label="Worksheet Name", value="comb"),
      radioButtons("outtyp", "Output Choice:",
               c("Save to file" = "sfl",
                 "Send to GDocs" = "gdoc")),
      hr(),
      helpText("comb_archive_[date]: archive of old comb file"),
      helpText("comb_new: new comb file. delete old file and rename comb"),
      helpText(sprintf("Code updated %s", update_dt))
    ),
    mainPanel(
      h2('New Students Added to Comb'),
      dataTableOutput('newstus'),
      h2('New IEPs added to Existing Students in Comb'),
      dataTableOutput('newieps'),
      h2('IEPs in Comb with Different Data in New Sheet'),
      dataTableOutput('chgieps'),
      h2('File location'),
      textOutput("flloc")
    )
    
    
  )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
   
  output$newstus <- renderDataTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    gs_auth()
    # gs_webapp_auth_url()
    withProgress(message="Working", value=0, {
      inc <- function(str=""){
        n=4
        incProgress(1/n, detail = str)
      }
      
      if(F) ss <- gs_url("https://docs.google.com/spreadsheets/d/1Es_RC8SRopbvojrXxk_PRxyuQknG-4y9NRoY5ZmLr-s")
      ss <- gs_url(paste0("https://docs.google.com/spreadsheets/d/", input$gs))
      inc("connecter to sheet")
      ## Read in current comb sheet and archive a copy
      asd <- function(x) as.Date(x, "%m/%d/%Y")
      if(F) {
        alld1 <- gs_read(ss, "comb")
        alld2 <- gs_read(ss, "comb_archived_2017_11_19")
        alld3 <- gs_read(ss, "comb_archived_2017_11_12")
        alld4 <- gs_read(ss, "comb_archived_2017_10_30")
        alld <- gs_read(ss, "comb")
        dt1=as.Date(alld$iep_end_dt, "%m/%d/%y")
        dt2=as.Date(alld$iep_end_dt)
        alld$iep_end_dt= coalesce(dt1, dt2)
        alld <- unique(alld)
      }
      alld <- gs_read(ss, input$sht)
      inc("comb loaded")
      arch_sheet <- sprintf("comb_archived_%s", format(Sys.Date(), "%Y_%m_%d"))
      if(arch_sheet %in% ss$ws$ws_title) {
        arch_sheet_v <- paste0(arch_sheet, "_v")
        curs <- grep(arch_sheet_v, ss$ws$ws_title, value=T)
        suffix <- max(as.numeric(gsub(arch_sheet_v, "", curs)), na.rm=T)
        arch_sheet <- paste0(arch_sheet_v, ifelse(suffix==-Inf, 1, suffix+1))
      }
      if(input$outtyp=="gdoc") {
        gs_ws_new(ss, ws_title=arch_sheet, input=alld, row_extent=nrow(alld))
      } else if(input$outtyp == "sfl") {
        gs_ws_new(ss, ws_title=arch_sheet, input=data.frame(id="placeholder"))
        write.csv(alld, paste0(arch_sheet, ".csv"), row.names = FALSE)
      }
      inc("comb archived")
      if(F) {
        newp <- read.csv("C:/Users/jy70/Downloads/QR_6955283773237741651.csv", stringsAsFactors=F)
        newp <- read.csv("/Users/MBP1/Dropbox/BJ/SLP Data/QR_8271924928357067777.csv", stringsAsFactors=F)
      } 
      
      newp <- read.csv(inFile$datapath, stringsAsFactors=F)
      newp <- newp %>% rename_(lasid=grep("lasid", names(newp), ignore.case = T, value=T))
      # if(!"current" %in% names(alld)) alld$current=1
      if("character" %in% class(alld$iep_end_dt)) {
        dt1=as.Date(alld$iep_end_dt, "%m/%d/%y")
        dt2=as.Date(alld$iep_end_dt)
        alld$iep_end_dt= coalesce(dt1, dt2)
      }
      if("integer" == class(alld$iep_end_dt_manual)) {
        alld$iep_end_dt_manual <- as.Date(alld$iep_end_dt_manual, origin="1970-01-01")
      } else if("character" == class(alld$iep_end_dt_manual)) {
        alld$iep_end_dt_manual <- as.Date(alld$iep_end_dt_manual, "%m/%d/%y")
      }
      ## only keep the most recent iep noted in the new data
      # nms <- c('lasid', 'Name', 'School', 'Grade', 'consult', 'push_in', 'push_out_11', 'push_out_grp', 'Homeroom', 'classroom', 'Homeroom_Teacher', 'teacher', 'Home_Lang', 'iep_start_dt', 'iep_end_dt', 'next_iep_review', 'next_iep_eval', paste0(rep(c("consult", "push_in", "push_out_11", "push_out_grp"), rep(3, 4)), rep(c("_freq", "_dur", "_days"), 4)))
      
      current_alld <- alld %>% select(lasid, current_dt=iep_end_dt) %>% arrange(lasid, desc(current_dt)) %>% filter(!duplicated(lasid))
      rec_only <- newp %>% group_by(lasid) %>% 
        summarise(EndDate = max(as.Date(EndDate, "%m/%d/%Y"))) %>% 
        left_join(newp %>% mutate(EndDate=as.Date(EndDate, "%m/%d/%Y")), by=c("lasid", "EndDate")) %>% 
        left_join(current_alld, by="lasid") %>% filter(is.na(current_dt) | EndDate >= current_dt) %>% 
        select(-current_dt)
      
      newp_cols <- c(Name="Name", School="Name.1", Homeroom="Homeroom", 
                     Home_Lang="NativeLanguage", Sped.Disability="Sped.Disability")
      newp_dts <- c(iep_start_dt="StartDate", iep_end_dt="EndDate",
                    next_iep_review="Next.IEP.review", next_iep_eval="Next.IEP.eval")
      
      newp_cols <- c(newp_cols, newp_dts)[c(newp_cols, newp_dts) %in% names(rec_only)]
      ## create text services info
      
      ind_details <- rec_only %>% group_by(lasid) %>% mutate_at(newp_cols, function(x) x[!is.na(x)][1]) %>% 
        ungroup %>% filter(!duplicated(lasid)) %>% .[c("lasid", names(newp_cols))]
      
      update_d <- rec_only %>% group_by(lasid) %>% do(mkrw(.)) %>% 
        left_join(ind_details[c("lasid", names(newp_cols))])
      
      day5_ids <- c("23514", "23434", "34070")
      
      for(vr in c("consult", "push_in", "push_out_11", "push_out_grp")){
        update_d[[vr]][!(update_d$lasid %in% day5_ids)] <- gsub(" 0 days", " 30 days", update_d[[vr]][!(update_d$lasid %in% day5_ids)])
        update_d[[vr]][update_d$lasid %in% day5_ids] <- gsub(" 0 days", " 5 days", update_d[[vr]][update_d$lasid %in% day5_ids])
      }
      # update_d$consult
      
      ## keep only data that does not already exist in the comb file
      ## should we also get this to update the services for subjects alread in comb?
      new_d <- update_d %>% anti_join(alld %>% select(lasid, iep_end_dt), by=c("lasid", "iep_end_dt")) %>% mutate(type="IEP")
      
      # update_d %>% select(lasid, iep_end_dt, push_out_grp_new=push_out_grp, consult_new=consult, push_in_new=push_in, push_out_11_new=push_out_11) %>% 
        # right_join(alld, by=c("lasid", "iep_end_dt"))
      
      alld_updates_1 <- alld %>% 
        left_join(update_d %>% select(lasid, iep_end_dt, push_out_grp_new=push_out_grp, 
                                      consult_new=consult, push_in_new=push_in, push_out_11_new=push_out_11) %>% 
                    mutate(upd=T), by=c("lasid", "iep_end_dt")) %>%
        mutate(updd=ifelse(is.na(upd), F, T)) %>% select(-upd)
      
      chkfxn <- function(x,y) (is.na(y) & !is.na(x)) | (!is.na(y) & is.na(x)) | (!is.na(x) & !is.na(y) & x != y)
      
      alld_updates <- alld_updates_1 %>% filter(current==1 & updd) %>% 
        filter(chkfxn(push_out_grp_new, push_out_grp) | 
                 chkfxn(consult_new, consult) | 
                 chkfxn(push_in_new,push_in) | 
                 chkfxn(push_out_11_new, push_out_11)) %>%
        mutate(push_out_grp=push_out_grp_new, consult=consult_new, push_in=push_in_new, push_out_11=push_out_11_new) %>%
        bind_rows(alld_updates_1 %>% filter(updd) %>% 
                    mutate(chgdl = chkfxn(push_out_grp_new, push_out_grp) | 
                             chkfxn(consult_new, consult) | 
                             chkfxn(push_in_new,push_in) | 
                             chkfxn(push_out_11_new, push_out_11),
                           chgd = as.numeric(chgdl),
                           current=if_else(chgd==1, as.integer(0), current),
                           iep_end_dt_manual=if_else(chgd==1, Sys.Date(), as.Date(NA)))) %>%
        bind_rows(alld_updates_1 %>% filter(!updd)) %>% 
        select(-push_out_grp_new, -consult_new, -push_in_new, -push_out_11_new, -chgd, -updd)
      
      ## for subjects already in comb we add in new IEP info and take most info from previous comb entry
      new_cols <- c('lasid', 'consult_freq', 'consult_dur', 'consult_days', 'push_in_freq', 'push_in_dur',
                    'push_in_days', 'push_out_11_freq', 'push_out_11_dur', 'push_out_11_days', 'push_out_grp_freq', 
                    'push_out_grp_dur', 'push_out_grp_days', 'push_out_grp', 'consult', 'push_out_11', 'push_in', 
                    'iep_start_dt', 'iep_end_dt', 'next_iep_eval', 'next_iep_review', 'type')
      new_cols <- new_cols[new_cols %in% names(new_d)]
      add_iep <- new_d %>% .[new_cols] %>% 
        inner_join(alld %>% select(lasid, Name, School, Description, classroom, Homeroom, 
                                   Home_Lang, Grade, service_provider, prog_name, teacher,
                                   Sped.Disability) %>% unique(), by="lasid") %>%
        ungroup %>% mutate(current=1)
      
      ## for new students we can only use the new data
      add_stu <- new_d %>% anti_join(alld, by="lasid") %>% ungroup %>% mutate(current=1)
      
      ## out schools mean services have been permanently removed
      out_schools <- c('discharged', 'discontinue', 'discontinued', 'disenrolled', 
                       'does not exist', 'dropped', 'exited', 'exited from 504', 
                       'exited from special ed', 'exited from sped', 'fiscal responsibility',
                       'moved', 'moved to lynn 12/17', 'moved?', 'never showed up', 'northeast',
                       'notheast', 'outplaced', 'rejected iep', 'temporarily discontinued')
      
      ## combine all data and write as comb_new
      ## go to the google sheet and delete comb and rename comb_new to comb
      new_comb <- bind_rows(alld_updates %>% mutate(current=ifelse(current==1 & lasid %in% add_iep$lasid, as.integer(0), current)),
                            add_iep %>% mutate_at(vars(iep_start_dt, next_iep_eval, next_iep_review), asd), 
                            add_stu %>% mutate_at(vars(iep_start_dt, next_iep_eval, next_iep_review), asd)) %>% 
        arrange(lasid, iep_end_dt) %>% 
        mutate(Grade = ifelse(!is.na(as.numeric(Grade)), paste0("Gr ", Grade), Grade)) %>% 
        group_by(lasid) %>% 
        mutate(chkout_school = any(tolower(School) %in% out_schools)) %>% 
        ungroup %>% 
        mutate(current= ifelse(chkout_school, 0, current)) %>% 
        select(-chkout_school) %>% 
        arrange(lasid, -current, iep_start_dt)
      
      
      inc("saving comb_new")
      comb_nm <- "comb_new"
      if("comb_new" %in% ss$ws$ws_title) {
        comb_nm_v <- "comb_new_v"
        curs <- grep(comb_nm_v, ss$ws$ws_title, value=T)
        suffix <- max(as.numeric(gsub(comb_nm_v, "", curs)), na.rm=T)
        comb_nm <- paste0(comb_nm_v, ifelse(suffix==-Inf, 1, suffix+1))
      }
      if(input$outtyp=="gdoc") {
        gs_ws_new(ss, ws_title=comb_nm, input=new_comb, row_extent=nrow(new_comb))
      } else if(input$outtyp == "sfl") {
        gs_ws_new(ss, ws_title=comb_nm, input=data.frame(id="placeholder"))
        write.csv(new_comb, "comb_new.csv", row.names = FALSE)
        output$flloc <- renderText({ 
          getwd()
        })
      }

      ## new ieps added
      newieps <- new_comb %>% filter(lasid %in% add_iep$lasid) %>% group_by(lasid) %>% 
        arrange(lasid, iep_end_dt) %>% 
        summarise(n_ieps = n(), 
                  iep_old_date=iep_end_dt[n_ieps-1], 
                  iep_new_dt=iep_end_dt[n_ieps], 
                  School=School[n_ieps],
                  Homeroom=Homeroom[n_ieps]) %>% 
        select(-n_ieps) %>% arrange(School)
      
      ## new students added
      newstus <- add_stu %>% select(lasid, School, Homeroom) %>% arrange(School)
      
      ## iep updates -- should show all new ieps that have same date as old but with different services recommended
      chgieps <- new_d %>% select(lasid, iep_end_dt, push_out_grp_new=push_out_grp, 
                                  consult_new=consult, push_out_11_new=push_out_11, 
                                  push_in_new=push_in) %>% 
        inner_join(alld %>% select(lasid, iep_end_dt, push_out_grp, consult, push_out_11, push_in), by=c("lasid", "iep_end_dt"))
      if(nrow(chgieps) > 0){
        output$chgieps <- renderDataTable(chgieps %>% filter(push_out_grp_new!=push_out_grp | consult_new!=consult | push_out_11_new!=push_out_11 | push_in_new!=push_in))
      } else {
        output$chgieps <- renderDataTable(data_frame())
      }
      
      output$newieps <- renderDataTable(newieps)
      newstus
    })
    
  })
})

# Run the application 
shinyApp(ui = ui, server = server)


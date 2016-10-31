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
      hr(),
      helpText("comb_archive_[date]: archive of old comb file"),
      helpText("comb_new: new comb file. delete old file and rename comb")
    ),
    mainPanel(
      h2('New Students Added to Comb'),
      dataTableOutput('newstus'),
      h2('New IEPs added to Existing Students in Comb'),
      dataTableOutput('newieps'),
      h2('IEPs in Comb with Different Data in New Sheet'),
      dataTableOutput('chgieps')
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
      if(F) alld <- gs_read(ss, "comb")
      alld <- gs_read(ss, input$sht)
      inc("comb loaded")
      arch_sheet <- sprintf("comb_archived_%s", format(Sys.Date(), "%Y_%m_%d"))
      if(arch_sheet %in% ss$ws$ws_title) {
        curs <- grep(arch_sheet, ss$ws$ws_title, value=T)
        suffix <- max(as.numeric(substr(curs, nchar(curs), nchar(curs))), na.rm=T)
        arch_sheet <- paste(arch_sheet, ifelse(suffix==-Inf, 1, suffix+1), sep="_v")
      }
      gs_ws_new(ss, ws_title=arch_sheet, input=alld, trim=T)
      inc("comb archived")
      if(F) newp <- read.csv("C:/Users/jy70/Downloads/data_pull_20161021.csv", stringsAsFactors=F)
      newp <- read.csv(inFile$datapath, stringsAsFactors=F)
      newp <- newp %>% rename_(lasid=grep("lasid", names(newp), ignore.case = T, value=T))
      if(!"current" %in% names(alld)) alld$current=1
      if(!"Date" %in% class(alld$iep_end_dt)) alld$iep_end_dt=as.Date(alld$iep_end_dt, "%m/%d/%Y")
      ## only keep the most recent iep noted in the new data
      # nms <- c('lasid', 'Name', 'School', 'Grade', 'consult', 'push_in', 'push_out_11', 'push_out_grp', 'Homeroom', 'classroom', 'Homeroom_Teacher', 'teacher', 'Home_Lang', 'iep_start_dt', 'iep_end_dt', 'next_iep_review', 'next_iep_eval', paste0(rep(c("consult", "push_in", "push_out_11", "push_out_grp"), rep(3, 4)), rep(c("_freq", "_dur", "_days"), 4)))
      
      current_alld <- alld %>% select(lasid, current_dt=iep_end_dt) %>% arrange(lasid, desc(current_dt)) %>% filter(!duplicated(lasid))
      rec_only <- newp %>% group_by(lasid) %>% summarise(EndDate = max(as.Date(EndDate, "%m/%d/%Y"))) %>% 
        left_join(newp %>% mutate(EndDate=as.Date(EndDate, "%m/%d/%Y")), by=c("lasid", "EndDate")) %>% 
        left_join(current_alld, by="lasid") %>% filter(EndDate >= current_dt) %>% select(-current_dt)
      
      ## create text services info
      update_d <- rec_only %>% group_by(lasid) %>% do(mkrw(.)) %>% 
        left_join(rec_only %>% filter(!duplicated(lasid)) %>% select(lasid, Name, School=Name.1, Homeroom, Home_Lang=NativeLanguage, iep_start_dt=StartDate, iep_end_dt=EndDate, next_iep_review=Next.IEP.review, next_iep_eval=Next.IEP.eval), by="lasid")
      
      ## keep only data that does not already exist in the comb file
      ## should we also get this to update the services for subjects alread in comb?
      new_d <- update_d %>% anti_join(alld %>% select(lasid, iep_end_dt), by=c("lasid", "iep_end_dt")) %>% mutate(type="IEP")
      
      update_d %>% select(lasid, iep_end_dt, push_out_grp_new=push_out_grp, consult_new=consult, push_in_new=push_in, push_out_11_new=push_out_11) %>% 
        right_join(alld, by=c("lasid", "iep_end_dt"))
      
      alld_updates_1 <- alld %>% left_join(update_d %>% select(lasid, iep_end_dt, push_out_grp_new=push_out_grp, consult_new=consult, push_in_new=push_in, push_out_11_new=push_out_11) %>% 
                                             mutate(upd=T), by=c("lasid", "iep_end_dt")) %>% mutate(updd=ifelse(is.na(upd), F, T)) %>% select(-upd)
      
      chkfxn <- function(x,y) (is.na(y) & !is.na(x)) | (!is.na(y) & is.na(x)) | (!is.na(x) & !is.na(y) & x != y)
      
      alld_updates <- alld_updates_1 %>% filter(current==1 & updd) %>% filter(chkfxn(push_out_grp_new, push_out_grp) | chkfxn(consult_new, consult) | chkfxn(push_in_new,push_in) | chkfxn(push_out_11_new, push_out_11)) %>%
        mutate(push_out_grp=push_out_grp_new, consult=consult_new, push_in=push_in_new, push_out_11=push_out_11_new) %>%
        bind_rows(alld_updates_1 %>% filter(updd) %>% mutate(chgd = chkfxn(push_out_grp_new, push_out_grp) | chkfxn(consult_new, consult) | chkfxn(push_in_new,push_in) | chkfxn(push_out_11_new, push_out_11),
                                            current=ifelse(chgd, 0, current),
                                            iep_end_dt_manual=ifelse(chgd, NA, Sys.Date()))) %>%
        bind_rows(alld_updates_1 %>% filter(!updd)) %>% 
        select(-push_out_grp_new, -consult_new, -push_in_new, -push_out_11_new, -chgd, -updd)
      
      ## for subjects already in comb we add in new IEP info and take most info from previous comb entry
      add_iep <- new_d %>% select(lasid, consult_freq, consult_dur, consult_days, push_in_freq, push_in_dur, push_in_days, push_out_11_freq, push_out_11_dur, push_out_11_days, push_out_grp_freq, push_out_grp_dur, push_out_grp_days, push_out_grp, consult, push_out_11, push_in, Name, School, Homeroom, Home_Lang, iep_start_dt, iep_end_dt, next_iep_eval, next_iep_review, type) %>% 
        inner_join(alld %>% select(lasid, Description, classroom, Grade, service_provider), by="lasid") %>% ungroup %>% mutate(current=1)
      
      ## for new students we can only use the new data
      add_stu <- new_d %>% anti_join(alld, by="lasid") %>% ungroup %>% mutate(current=1)
      
      ## combine all data and write as comb_new
      ## go to the google sheet and delete comb and rename comb_new to comb
      new_comb <- bind_rows(alld_updates %>% mutate(current=ifelse(current==1 & lasid %in% add_iep$lasid, 0, current)), add_iep, add_stu) %>% arrange(lasid, iep_end_dt)
      inc("saving comb_new")
      if("comb_new" %in% ss$ws$ws_title) {
        curs <- grep("comb_new", ss$ws$ws_title, value=T)
        suffix <- max(as.numeric(substr(curs, nchar(curs), nchar(curs))), na.rm=T)
        gs_ws_new(ss, ws_title=paste(arch_sheet, ifelse(suffix==-Inf, 1, suffix+1), sep="_v"), input=new_comb, trim=T)
      } else {
        gs_ws_new(ss, ws_title="comb_new", input=new_comb, trim=T)
      }
      
      ## new ieps added
      newieps <- new_comb %>% filter(lasid %in% add_iep$lasid) %>% group_by(lasid) %>% arrange(lasid, iep_end_dt) %>% summarise(n_ieps = n(), iep_old_date=iep_end_dt[n_ieps-1], iep_new_dt=iep_end_dt[n_ieps], School=School[n_ieps], Homeroom=Homeroom[n_ieps]) %>% select(-n_ieps) %>% arrange(School)
      
      ## new students added
      newstus <- add_stu %>% select(lasid, School, Homeroom) %>% arrange(School)
      
      ## iep updates -- should show all new ieps that have same date as old but with different services recommended
      chgieps <- new_d %>% select( lasid, iep_end_dt, push_out_grp_new=push_out_grp, consult_new=consult, push_out_11_new=push_out_11, push_in_new=push_in) %>% 
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


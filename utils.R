customCSS <- function() {
  tags$head(tags$style(HTML('
.main-header .logo span {
  display: inline-block;
  line-height: normal;
  vertical-align: middle;
  font-size: smaller;
  padding-left: inherit;
}

.skin-blue .control-label {
  color: #FFFFFF
}

.skin-blue .radio > label {
  color: #FFFFFF
}

#searchButton {
  background-color: rgb(24,188,156);
  color: #FFFFFF;
  padding:5px 30px;
  width: 50%;
  position: relative;
  left: 40px;
}

.skin-blue .sidebar h3 {
  color: #FFFFFF;
  position: relative;
  left: 40px;
}


/* minor thing to get results table to fill sidebar fully */
.table {
  color: rgb(33,37,41);
  width: 95% !important;
  position: relative;
  left: 5px;
}
')))
}


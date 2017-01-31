# Test gmailR package.
# Goals: send a text from R to my cellphone
library(devtools)
install_github("gmailR", "trinker")
# This option, and "mailR", both rely on "rJava". Apparently, installing and
# loading "rJava" is hella complicated. See:
# http://stackoverflow.com/questions/7019912/using-the-rjava-package-on-win7-64-bit-with-r
# or
# http://stackoverflow.com/questions/27661325/unable-to-load-rjava-on-r

# Ok, so the other package to try is called "sendmailR", and it doesn't appear
# to depend on "rJava" from what I've found.
install.packages("sendmailR")
library(sendmailR)
sendmail(from = sprintf("<gabriel.j.odom@gmail.com>", Sys.info()[4]),
         # sprintf("<sendmailR@\\%s>", Sys.info()[4])
         to = "<gabriel_odom@baylor.edu>", #, "mandy_hering@baylor.edu"
         subject = "Test Email from R",
         body = list("Come here Watson, I need you."),
         control = list(smtpServer = "ASPMX.L.GOOGLE.COM"))
# This has been a giant waste of time. Back to rJava

# rJava
install.packages('rJava', .libPaths()[1], 'http://www.rforge.net/')
library(rJava)
install.packages("mailR", dep = TRUE)
library(mailR)
?mailR::send.mail

# We can't send mail from accounts with two-factor identification, so my gmail
# is out
send.mail(from = "gabriel.j.odom@gmail.com",
          to = c("gabriel_odom@baylor.edu", "Recipient 2 <mandy_hering@baylor.edu>"),
          # replyTo = c("Reply to someone else <someone.else@gmail.com>")
          subject = "Test Email from R",
          body = "Mr. Watson -- Come here -- I want to see you",
          smtp = list(host.name = "smtp.gmail.com",
                      port = 465,
                      user.name = "gabriel.j.odom",
                      passwd = "Three.1415",
                      ssl = TRUE),
          authenticate = TRUE,
          send = TRUE)

# MY GOD IT WORKS
send.mail(from = "gabriel_odom@baylor.edu",
          to = "gabriel.j.odom@gmail.com",
          subject = "Test Email from R",
          body = "Mr. Watson -- Come here -- I want to see you",
          smtp = list(host.name = "smtp.office365.com",
                      port = 587,
                      user.name = "gabriel_odom@baylor.edu",
                      passwd = "bVkwAV3(",
                      tls = TRUE),
          authenticate = TRUE,
          send = TRUE)

# Try the text message
send.mail(from = "gabriel_odom@baylor.edu",
          to = "8503822279@vtext.com",
          subject = "Test Email from R",
          body = "Mr. Watson -- Come here -- I want to see you",
          smtp = list(host.name = "smtp.office365.com",
                      port = 587,
                      user.name = "gabriel_odom@baylor.edu",
                      passwd = "bVkwAV3(",
                      tls = TRUE),
          authenticate = TRUE,
          send = TRUE)
# It works!

# To review, we'll need the following: a valid MS office email address and
# password (using host.name = "smtp.office365.com" and port = 587), or a valid
# gmail address and password WITHOUT two-stage authentication (using host.name
# = "smtp.gmail.com" and port = 465); and either a) the recipients' email
# addresses or b) the recipients' 10-digit phone numbers and cell carriers. I
# still need to create the matching list of cell carriers to email accounts.

send.mail(from = "gabriel_odom@baylor.edu",
          to = "7202536323@vtext.com",
          subject = "Test Email from R",
          body = "Hi Kate,
          This is a text email from R. Please let me know if you recieved it.
          Gabriel",
          smtp = list(host.name = "smtp.office365.com",
                      port = 587,
                      user.name = "gabriel_odom@baylor.edu",
                      passwd = "bVkwAV3(",
                      tls = TRUE),
          authenticate = TRUE,
          send = TRUE)

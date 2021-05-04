runGitHub( "pruebadashCOVID", "hhsebas93")
install.packages('rsconnect')
rsconnect::setAccountInfo(name='hhsebas93',
                          token='66A92F2EDCE2D30B9930AA53D8A5A8DD',
                          secret='XnVj95WjFr4d/9hH7hmxz4ZT9JRlaP32sYSkpfJd')
library(rsconnect)
rsconnect::deployApp('~/Pruebas Dashboard/pruebadashCOVID')

#install.packages('rvest')
library('rvest')
library('stringr')
library('tidyverse')

siteUrl = 'https://www.avanza.se'
startUrl = '/placera/forum/start.150000.html'
url = paste(siteUrl, startUrl, sep="")
waitTime = runif(200, 3, 9)
forumData = data.frame()

# För att scrapea alla sidor: 
# 1. börja på sista sidan
# 2. hämta href för föregående sida
# 3. använd som url
# 4. upprepa tills första sidan är nådd

for (i in 1:200){
    Sys.sleep(waitTime[i])
    html = read_html(url)
    
    forumTabell = html_table(html, header = TRUE)[[1]]  # Table
    forumTabell['Text'] = html_nodes(html, "[class='forumBox clearFix lhNormal forumPostText SText']") %>% html_text()    # Posts
    forumTabell['Subject'] = html_nodes(html, "[class='forumBoxHeader']") %>% html_text()   # Subforum
    
    pages = html_nodes(html, "[class='tablePager']") %>% html_attr('href')  # Next page
    
    # Clean Subject-column
    newColumns = c('mainTopic', 'subTopic', 'postCaption')
    forumTabell[newColumns] = forumTabell[1:nrow(forumTabell), 'Subject'] %>%
                                str_split("/\r\n") %>%
                                    unlist() %>%
                                        str_replace_all("[\r\n\t\t\t\t]" , "") %>%
                                            trimws(which = "both", whitespace = "[[:space:]]") %>%
                                                matrix(nrow = nrow(forumTabell), ncol = 3, byrow = TRUE)
    forumTabell = forumTabell[15:1, ]
    forumData = rbind(forumData, forumTabell)   # Bind data to dataframe
    url = paste(siteUrl, pages[1], sep="")  # Create next url
}

rownames(forumData) = NULL

forumDataUnique = unique(forumData)
forumDataUnique %>% 
    plyr::count('Forum') %>%
        arrange(desc(freq))

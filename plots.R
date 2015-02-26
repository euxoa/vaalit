library(dplyr)
library(tidyr)
library(RColorBrewer)
library(ggplot2)


sol6b <- readRDS("sol6b.rds")
d <- readRDS("d.rds")

attach(sol6b)

comp.names <- c("C.econlib", "C.nat", "C.soslib", "C.cons", "C.green", "C.tradit")
pol <- cbind(d %>% select(name, party, district), theta %>% data.frame %>% setNames(comp.names))


if (F) {
  for (cname in comp.names) {
    cat("\nComp ", cname, "\n")
    print(pol %>% arrange_(paste("-", cname, sep="")) %>% select_("name", "party", cname) %>% head(20))
  }
  
  hist(p.v, n=100)
  heatmap(theta)
}



party.plot <- function(prty, title=NULL, districts=c("Uusimaa", "Helsinki")) {
  pol %>% filter(district %in% districts & party==prty) %>% 
    arrange(party) %>% select(name, matches("^C")) %>%
    gather(comp, p, -name) %>% 
    ggplot(aes(x=comp, y=name, fill=p)) + geom_tile() + 
    scale_fill_gradientn(colours = rev(brewer.pal(9, "YlGnBu"))) + 
    labs(x="", y="") + scale_x_discrete(expand = c(0, 0)) + 
    scale_y_discrete(expand = c(0, 0)) + theme(axis.ticks = element_blank()) +
    ggtitle(ifelse(is.null(title), prty, title))
}

pdf("Hki-Uusimaa-partyplots.pdf")
names(sort(table(d$party), decr=T)) %>% 
  sapply(., function (i) print(party.plot(i)))
#dev.off()

#pdf("Comp-response-plots.pdf")
(function (k) 
  phi[k,,] %>% data.frame(q=rownames(.), .) %>% gather(reply, p, -q) %>%
   cbind(comp=comp.names[k], .)) %>%
  sapply(1:K, ., simplify=F) %>%
  rbind_all %>%
  ggplot(aes(x=reply, y=q, fill=p)) + geom_tile() + 
  scale_fill_gradientn(colours = rev(brewer.pal(9, "YlGnBu"))) + 
  labs(x="", y="") + scale_x_discrete(expand = c(0, 0)) + 
  scale_y_discrete(expand = c(0, 0)) + theme(axis.ticks = element_blank(), legend.position="none") +
  facet_grid(~ comp) + 
  ggtitle("Responses of components")
#dev.off()

pol %>% group_by(party) %>% 
  summarise_each(funs(mean), matches("^C")) %>% gather(comp, p, -party) %>% 
  ggplot(aes(x=comp, y=party, fill=p)) + geom_tile() +
  scale_fill_gradientn(colours = rev(brewer.pal(9, "YlGnBu"))) + 
  theme_bw(14) +
  labs(x="", y="") + scale_x_discrete(expand = c(0, 0)) + 
  scale_y_discrete(expand = c(0, 0)) + theme(axis.ticks = element_blank()) +
  ggtitle("By party (whole country)")

pol %>% filter(district != "default") %>% group_by(district) %>% 
  summarise_each(funs(mean), matches("^C")) %>% gather(comp, p, -district) %>% 
  ggplot(aes(x=comp, y=district, fill=p)) + geom_tile() +
  scale_fill_gradientn(colours = rev(brewer.pal(9, "YlGnBu"))) + 
  theme_bw(14) +
  labs(x="", y="") + scale_x_discrete(expand = c(0, 0)) + 
  scale_y_discrete(expand = c(0, 0)) + theme(axis.ticks = element_blank()) +
  ggtitle("By disctrict")

dev.off()

dgl <- 
  list(c("Kaakkois-Suomi", "Savo-Karjala", "Keski-Suomi"), 
       c("Varsinais-Suomi", "Häme", "Pirkanmaa", "Satakunta"),
       c("Vaasa", "Oulu", "Lappi"),
       c("Helsinki", "Uusimaa"))

pdf("Partyplots-all.pdf")
sapply(dgl, 
       function (districts)
        names(sort(table(d %>% filter(district %in% districts) %>% .$party %>% as.character), decr=T)) %>% 
          sapply(., function (i) print(party.plot(i, districts=districts, 
                                                  title=paste(paste(districts, collapse=", "), i, sep=": ")))))
dev.off()


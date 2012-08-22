
<<>>=

### Another plot

# Joint both tables
t.as <- data.frame(rbind(t.sex, t.age, t.employ, t.region))

# Call joined tables
t.as

# Create a new colloum to identify the categories

t.as <- data.frame(cat = c(rep("sex", length(levels(t.sex$cov))),
                           rep("age", length(levels(t.age$cov))),
                           rep("employ", length(levels(t.employ$cov))),
                           rep("region", length(levels(t.region$cov)))
                           ),
                   t.as)

limits <- aes(ymax = (t.as$ul * 100), ymin = (t.as$ll) * 100)

# Graphics prevalence by diferent variables levels in one plot


P <- 
  ggplot(t.as, aes(x = cov, y = prev * 100, fill = cat, colour = cat)) +  
  stat_identity(geom = "point", size = 5) + 
  geom_errorbar(limits, colour = "Black", size = 0.90, width = 0.25) +
  opts(axis.text.x = theme_text(colour = 'grey', angle = 0, 
                                size = 12, hjust = 0, vjust = 0, face = ),
       axis.text.y = theme_text(colour = 'grey', angle = 0, 
                                size = 10, hjust = 0, vjust = 0, face = )) +
                                  opts(panel.border = theme_blank()) +
                                  opts(axis.line = theme_segment(colour = "black"))

P

P + facet_grid(facet = . ~ cat, margins = FALSE, scales = "free_x", space = "free",
               labeller = , as.table = TRUE, shrink = TRUE, drop = TRUE) +
  opts(axis.text.x = theme_text(angle = 45), legend.position = "none") +
  geom_rect(data = t.as, aes(fill = cat), xmin = -Inf, xmax = Inf,
            ymin = -Inf, ymax = Inf, alpha = 0.085) +
  opts(title = "Child lifetime asthma prevalence by covariates, 2008-2010")
 

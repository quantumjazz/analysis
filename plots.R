theme_set(theme_bw())
theme_update(panel.grid.minor = element_line(size = 0.2), panel.grid.major = element_line(size = 0.2))
#theme_update(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
theme_update(text = element_text(size = 14, family="CMU Serif"))
theme_update(axis.ticks = element_line(size = 0.2))
theme_update(axis.line = element_blank())

monthly_ts %>% pivot_longer(c(m_demand, m_temp)) %>%
ggplot(aes(x = month, y = value)) +
geom_line(size = 0.3) +
facet_grid(vars(name), scales = "free_y", labeller = labeller(name = labels)) +
theme(axis.line = element_line(size = 0.2)) +
theme(panel.border = element_rect(size = 0.2)) +
theme(strip.background = element_rect(color = "black", fill = "white", size = 0.2)) +
theme(panel.background = element_rect(color = "black", size = 0.2)) +
ylab("") + xlab("Година")
ggsave("four_one.png", width = 18, height = 9, units = "cm", dpi = 600)


monthly_ts %>% ggplot(aes(x = m_temp, y = m_demand)) + geom_point(size = 0.5) +
xlab("Температура, °C") + ylab("Потребление, GWh") 
ggsave("four_two.png", width = 18, height = 9, units = "cm", dpi = 600)

monthly_ts %>% gg_subseries(m_demand, size = 0.1) +
xlab("Години") + ylab("GWh")
ggsave("four_five.png", width = 18, height = 9, units = "cm", dpi = 600)


components %>% pivot_longer(c(m_demand, trend, `season_1 year`, remainder)) %>%
ggplot(aes(x = month, y = value)) +
geom_line(size = 0.3) +
facet_grid(vars(name), scales = "free_y", labeller = labeller(name = labels)) +
theme(strip.background = element_rect(color = "black", fill = "white", size = 0.2)) +
theme(panel.background = element_rect(color = "black", size = 0.2)) +
ylab("") + xlab("Година")

ggsave("four_six.png", width = 18, height = 12, units = "cm", dpi = 600)


forecast(fit, h=12) %>% autoplot(size = 0.3, linetype = "dashed", color = "black") + autolayer(monthly_ts, size = 0.3) +
labs(x = "Година", y = "GWh")
ggsave("four_seven.png", width = 18, height = 9, units = "cm", dpi = 600)

monthly_ts %>% ggplot(aes(x = m_temp, y = m_demand)) + geom_point(size = 0.5) + 
geom_smooth(method = "lm", formula = y ~ ns(x, df=4), size = 0.3, color = "black") +
xlab("Температура, °C") + ylab("Потребление, GWh")
ggsave("four_eight.png", width = 18, height = 9, units = "cm", dpi = 600)

forecast(mdls, new_data = test) %>% autoplot(size = 0.3) + autolayer(monthly_ts, size = 0.3) + labs(x = "Година", y = "GWh")
ggsave("four_nine.png", width = 18, height = 9, units = "cm", dpi = 600)


timeseries %>%
ggplot(aes(x = datetime, y = demand)) +
geom_line(size = 0.2) + labs(x = "Година", y = "Товар, MW")
ggsave("four_ten.png", width = 18, height = 9, units = "cm", dpi = 600)

timeseries %>%
filter(dateyear > "2018-12-31" & dateyear < "2019-02-01") %>%
mutate(date = as_date(dateyear), time = hour(datetime)) %>%
ggplot(aes(x = time, y = demand)) +
geom_line(size = 0.2) + facet_calendar(~ date) + labs(x = "Час", y = "Товар, MW")
ggsave("four_eleven.png", width = 18, height = 9, units = "cm", dpi = 600)

timeseries %>%
filter(dateyear > "2019-04-30" & dateyear < "2019-06-01") %>%
mutate(date = as_date(dateyear), time = hour(datetime)) %>%
ggplot(aes(x = time, y = demand)) +
geom_line(size = 0.2) + facet_calendar(~ date) + labs(x = "Час", y = "Товар, MW")
ggsave("four_twelve.png", width = 18, height = 12, units = "cm", dpi = 600)

timeseries %>%
filter(dateyear > "2019-07-31" & dateyear < "2019-09-01") %>%
mutate(date = as_date(dateyear), time = hour(datetime)) %>%
ggplot(aes(x = time, y = demand)) +
geom_line(size = 0.2) + facet_calendar(~ date) + labs(x = "Час", y = "Товар, MW")
ggsave("four_thirteen.png", width = 18, height = 12, units = "cm", dpi = 600)

timeseries %>% ggplot(aes(x = temp1, y = demand)) + geom_point(size=0.1) +
  xlab("Температура, °C") + ylab("Потребление, MW")
ggsave("four_sixteen.png", width = 18, height = 12, units = "cm", dpi = 600)
###
timeseries %>% ggplot(aes(x = datetime, y = demand)) + geom_line() + geom_line(aes(x = datetime, y = qdemand), color = "red")

timeseries %>% ggplot(aes(x = datetime, y = qddemand)) + geom_line()




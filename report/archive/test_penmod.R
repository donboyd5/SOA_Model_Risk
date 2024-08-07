
nsims <- 10000
nyears <- 50

scen1 <- expand_grid(sim=1:nsims, year=1:nyears) |> 
  mutate(ir=rnorm(nsims * nyears, .07, .10),
         scenario="scen1")

scen2 <- expand_grid(sim=1:nsims, year=1:nyears) |> 
  mutate(ir=rnorm(nsims * nyears, .07, .16),
         scenario="scen2")

stack <- bind_rows(scen1, scen2)

result <- stack %>%
  group_by(scenario) %>%
  summarise(
    nsims = n_distinct(sim),
    nyears = n_distinct(year),
    ir_matrix = list(matrix(ir, nrow = nsims, ncol = nyears, byrow = TRUE))
  )
result

result <- result  |> 
  mutate(
    penmod = map(seq_along(ir_matrix), function(matrix_element) {
      penmod(irates = matrix_element, nsims = nsims[.x], nyears = nyears[.x])
    })
  )

res2 <- result |> 
  mutate(penmod = purrr::map(seq_along(ir_matrix),
                               ~penmod(
                                 irates = ir_matrix[[.x]],
                                 nsims = nsims[.x],   # Ensures nsims is passed as a scalar
                                 nyears = nyears[.x]  # Ensures nyears is passed as a scalar
                               )))

res2


res2 |> 
  select(-ir_matrix) |> 
  unnest(cols = penmod) |> 
  group_by(scenario) |> 
  filter(year==max(year)) |> 
  summarise(maxyear=max(year),
            maxsim=max(sim),
            fr90=quantile(fr, probs=.90),
            fr50=median(fr),
            fr10=quantile(fr, probs=.10),
            irmean=mean(ir),
            irsd=sd(ir))



# old below here ----------------------------------------------------------



m <- result[1,]$ir_matrix[[1]]
penmod(nsims=10, nyears=5, irates=m)
result |> 
  mutate(penmod = purrr::map(
    ir_matrix,
    ~penmod(irates = .x, nsims = nsims, nyears = nyears)))

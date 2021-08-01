# Example functions to build a package around. 

require(tidyverse)


# Fake Data ---------------------------------------------------------------
set.seed(123)
N = 1000
dat <- tibble(x1 = rnorm(N),
              x2 = rnorm(N),
              x3 = rnorm(N),
              x4 = rnorm(N)) %>% 
  mutate(y = 1 + 2*x1 + -1*x2 + .5*x3 + rnorm(N,mean = 0,sd=5))


# Code 1: Tidy Data from LM model -------------------------------------

    mod <- lm(y ~ x1 + x2 + x3 + x4, data = dat)
    
    mod_sum <- summary(mod)
    
    model_table <- 
      mod_sum$coefficients %>% 
      as.data.frame() %>% 
      rownames_to_column(var = "variable") %>% 
      janitor::clean_names() %>% 
      mutate_if(is.numeric,function(x) round(x,3)) %>%
      mutate(variable = janitor::make_clean_names(variable)) %>% 
      mutate(significant = (abs(t_value) >= 1.96)) %>% 
      tibble()
    

# Code 2: jittered Coef Plot -----------------------------------------------------

    # Drop intercept
    model_table <- 
      model_table %>% 
      filter(!str_detect(variable,'intercept')) 
    
    # Take draws from a normal distribution (for the jittered band)
    n_sims = 500
    simulate_values <- NULL
    for( i in 1:nrow(model_table)){
      tibble(variable = model_table$variable[i],
             sim_val = rnorm(n_sims,mean = model_table$estimate[i],sd = model_table$std_error[i]),
             sig_color = case_when(
               model_table$significant[i] & sign(model_table$estimate[i]) == 1~"#e889df",
               model_table$significant[i]~"steelblue",
               T ~ "grey60"
             )
      ) %>% 
        bind_rows(simulate_values,.) -> simulate_values
    }

    
    # Generate plot
    model_table %>% 
      ggplot(aes(estimate,fct_reorder(variable,abs(estimate)))) + 
      geom_point() +
      geom_vline(xintercept = 0,lty=2) +
      geom_jitter(data = simulate_values,aes(x=sim_val,y=variable,color=sig_color),
                  alpha=.1,height = .075) +
      scale_color_identity() +
      geom_errorbarh(aes(xmin=estimate - std_error*1.96,
                         xmax=estimate + std_error*1.96),
                     color="grey30",height=.075,size=1) +
      geom_point(size=3,color="grey30") +
      ggrepel::geom_label_repel(aes(y=variable,x=estimate,
                                    label=ifelse(significant,paste0(estimate,"*"),estimate))
      ) +
      ggthemes::theme_hc() +
      labs(y="Variable",x="OLS Estimates")
      


# Mission -----------------------------------------------------------------

    # (1) Generalize this code as a function
    # (2) Build a package around code. 
    # (3) Test
    
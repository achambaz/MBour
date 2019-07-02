## ----include = FALSE, cache = FALSE--------------------------------------
options(digits = 3)

knitr::opts_chunk$set(
  comment = "#>",
  collapse = TRUE,
  cache = TRUE,
  out.width = "70%",
  fig.align = 'center',
  fig.width = 6, # used to have: fig.width = 12,
  fig.asp = 0.618, # used to have: fig.height = 4, 
  fig.show = "hold",
  fig.path = 'img/',
  size = "tiny",
  message = FALSE,
  warning = FALSE,
  warnings = FALSE
)

options(dplyr.print_min = 6, dplyr.print_max = 6)

## ----install-package, eval = FALSE---------------------------------------
## devtools::install_github("achambaz/tlride/tlrider")

## ----visible-setup, results = FALSE--------------------------------------
set.seed(54321) ## because reproducibility matters...
library(tidyverse)
library(caret)
library(ggdag)
library(tlrider)

## ----example-one, results = FALSE----------------------------------------
example(tlrider)

## ----example-two---------------------------------------------------------
ls()

## ----view-experiment-----------------------------------------------------
experiment

## ----draw-five-obs-------------------------------------------------------
(five_obs <- sample_from(experiment, n = 5))

## ----reveal-experiment---------------------------------------------------
relevant_features <- reveal(experiment)
names(relevant_features)

## ----show-QW-------------------------------------------------------------
relevant_features$QW

## ----show-Gbar-----------------------------------------------------------
relevant_features$Gbar

## ----show-qY-------------------------------------------------------------
relevant_features$qY

## ----show-Qbar-----------------------------------------------------------
relevant_features$Qbar

## ----show-sample-from----------------------------------------------------
relevant_features$sample_from

## ----exercise:visualize, eval = TRUE-------------------------------------
Gbar <- relevant_features$Gbar
Qbar <- relevant_features$Qbar
QW <- relevant_features$QW

features <- tibble(w = seq(0, 1, length.out = 1e3)) %>%
  mutate(Qw = QW(w),
         Gw = Gbar(w),
         Q1w = Qbar(cbind(A = 1, W = w)),
         Q0w = Qbar(cbind(A = 0, W = w)),
         blip_Qw = Q1w - Q0w)

features %>% select(-Qw, -Gw) %>%
  rename("Q(1,.)" = Q1w,
         "Q(0,.)" = Q0w,
         "Q(1,.) - Q(0,.)" = blip_Qw) %>%
  gather("f", "value", -w) %>%
  ggplot() +
  geom_line(aes(x = w, y = value, color = f), size = 1) +
  labs(y = "f(w)", title = bquote("Visualizing" ~ bar(Q)[0])) +
  ylim(NA, 1)

## ----my-own-experiment-a-------------------------------------------------
my_experiment <- LAW() ## creates an object of class 'LAW'
alter(my_experiment,   ## characterize its relevant features
      QW = function(W) {
        out <- rep_len(0, length(W))
        out[W == 0] <- 1/4
        out[W == 1] <- 3/4
        return(out)
      },
      Gbar = function(W) {
        out <- rep_len(0, length(W))
        out[W == 0] <- 1/3
        out[W == 1] <- 3/5
        return(out)
      },
      Qbar = function(AW) {
        probs <- matrix(c(1/2, 2/3, 7/8, 4/5), ncol = 2,
                        dimnames = list(c("A=0", "A=1"),
                                        c("W=0", "W=1")))
        probs[cbind(AW[, "A"] + 1, AW[, "W"] + 1)]
      },
      qY = function(obs) {
        probs <- matrix(c(1/2, 2/3, 7/8, 4/5), ncol = 2,
                        dimnames = list(c("A=0", "A=1"),
                                        c("W=0", "W=1")))
        probs <- probs[cbind(obs[, "A"] + 1, obs[, "W"] + 1)]
        obs[, "Y"] * probs + (1 - obs[, "Y"]) * (1 - probs)
      },
      sample_from = function(n) {
        ## preliminary
        n <- R.utils::Arguments$getInteger(n, c(1, Inf))
        ## 'QW', 'Gbar' and 'Qbar' features
        QW <- my_experiment$.QW
        Gbar <- my_experiment$.Gbar
        Qbar <- my_experiment$.Qbar
        ## sampling
        W <- rbinom(n, size = 1, prob = QW(1))
        A <- rbinom(n, size = 1, prob = Gbar(W))
        AW <- cbind(W = W, A = A)
        Y <- rbinom(n, size = 1, Qbar(AW))
        return(cbind(AW, Y = Y))
      })

## ----my-own-experiment-b-------------------------------------------------
(sample_from(my_experiment, 3))

## ----my-own-experiment-c-------------------------------------------------
obs <- sample_from(my_experiment, 1e4)
obs %>% as_tibble %>% group_by(W, A, Y) %>% summarize(how_many = n()) %>% ungroup
obs %>% as_tibble %>% group_by(W, A) %>% summarize(prob = mean(Y)) %>% ungroup

## ----approx-psi-0-a-one--------------------------------------------------
(psi_zero <- evaluate_psi(experiment))

## ----DAG, out.width = '70%', fig.align = 'center', fig.width = 8, fig.height = 6, fig.cap = '(ref:DAG)'----
dagify(
  Y ~ A + Y1 + Y0, A ~ W, Y1 ~ W, Y0 ~ W,
  labels = c(Y = "Actual reward",
             A = "Action",
             Y1 = "Counterfactual reward\n of action 1",
             Y0 = "Counterfactual reward\n of action 0",
             W = "Context of action"),
  coords = list(
    x = c(W = 0, A = -1, Y1 = 1.5, Y0 = 0.25, Y = 1),
    y = c(W = 0, A = -1, Y1 = -0.5, Y0 = -0.5, Y = -1)),
  outcome = "Y",
  exposure = "A",
  latent = c("Y0", "Y1")) %>% tidy_dagitty %>%
  ggdag(text = TRUE, use_labels = "label") + theme_dag_grey()

## ----approx-psi-zero-a-two-----------------------------------------------
B <- 1e5
ideal_obs <- sample_from(experiment, B, ideal = TRUE)
(psi_approx <- mean(ideal_obs[, "Yone"] - ideal_obs[, "Yzero"]))

## ----approx-psi-zero-b---------------------------------------------------
sd_approx <- sd(ideal_obs[, "Yone"] - ideal_obs[, "Yzero"])
alpha <- 0.05
(psi_approx_CI <- psi_approx + c(-1, 1) *
   qnorm(1 - alpha / 2) * sd_approx / sqrt(B))

## ----another-simulation--------------------------------------------------
another_experiment
reveal(another_experiment)
(two_obs_another_experiment <- sample_from(another_experiment, 2, h = 0))

## ----approx-psi-one------------------------------------------------------
(psi_Pi_zero <- evaluate_psi(another_experiment, h = 0))

## ----show-another-experiement-a------------------------------------------
another_experiment

## ----show-another-experiement-b------------------------------------------
reveal(another_experiment)$sample_from

## ----psi-approx-psi-one, fig.cap = '(ref:psi-approx-psi-one)'------------
approx <- seq(-1, 1, length.out = 1e2)
psi_Pi_h <- sapply(approx, function(t) {
  evaluate_psi(another_experiment, h = t)
})
slope_approx <- (psi_Pi_h - psi_Pi_zero) / approx
slope_approx <- slope_approx[min(which(approx > 0))]
ggplot() +
  geom_point(data = data.frame(x = approx, y = psi_Pi_h), aes(x, y),
             color = "#CC6666") +
  geom_segment(aes(x = -1, y = psi_Pi_zero - slope_approx,
                   xend = 1, yend = psi_Pi_zero + slope_approx),
               arrow = arrow(length = unit(0.03, "npc")),
               color = "#9999CC") +
  geom_vline(xintercept = 0, color = "#66CC99") +
  geom_hline(yintercept = psi_Pi_zero, color = "#66CC99") +
  labs(x = "h", y = expression(Psi(Pi[h]))) 

## ----yet-another-experiment----------------------------------------------
yet_another_experiment <- copy(another_experiment)
alter(yet_another_experiment,
      Qbar = function(AW, h){
        A <- AW[, "A"]
        W <- AW[, "W"]
        expit( logit( A * W + (1 - A) * W^2 ) + 
               h * (2*A - 1) / ifelse(A == 1,
                                      sin((1 + W) * pi / 6), 
                                      1 - sin((1 + W) * pi / 6)) *
               (Y - A * W + (1 - A) * W^2))
      })

## ----eic-one-------------------------------------------------------------
eic_experiment <- evaluate_eic(experiment)


## ----eic-two-------------------------------------------------------------
(eic_experiment(five_obs))

## ----eic-three, out.width = '80%', fig.cap = '(ref:eic-three)'-----------
crossing(w = seq(0, 1, length.out = 2e2),
	 a = c(0, 1),
	 y = seq(0, 1, length.out = 2e2)) %>%
  mutate(eic = eic_experiment(cbind(Y=y,A=a,W=w))) %>%
  ggplot(aes(x = w, y = y, fill = eic)) +
  geom_raster(interpolate = TRUE) +
  geom_contour(aes(z = eic), color = "white") +
  facet_wrap(~ a, nrow = 1,
             labeller = as_labeller(c(`0` = "a = 0", `1` = "a = 1"))) +
  labs(fill = expression(paste(D^"*", (P[0])(w,a,y))))

## ---- sigma0-------------------------------------------------------------
sigma0

## ----recover-slope-------------------------------------------------------
eic_another_experiment <- evaluate_eic(another_experiment, h = 0)
obs_another_experiment <- sample_from(another_experiment, B, h = 0)
vars <- eic_another_experiment(obs_another_experiment) *
  sigma0(obs_another_experiment)

sd_hat <- sd(vars)
(slope_hat <- mean(vars))
(slope_CI <- slope_hat + c(-1, 1) * qnorm(1 - alpha / 2) * sd_hat / sqrt(B))

## ----cramer-rao----------------------------------------------------------
obs <- sample_from(experiment, B)
(cramer_rao_hat <- var(eic_experiment(obs)))

## ----cramer-rao-another-experiment---------------------------------------
obs_another_experiment <- sample_from(another_experiment, B, h = 0)
(cramer_rao_Pi_zero_hat <-
   var(eic_another_experiment(obs_another_experiment)))

## ----evaluating-remainder------------------------------------------------
(evaluate_remainder(experiment, experiment))
(rem <- evaluate_remainder(experiment, another_experiment,
                           list(list(), list(h = 0))))

## ----iter----------------------------------------------------------------
iter <- 1e2

## ----cautionary----------------------------------------------------------
compute_irrelevant_estimator <- function(obs) {
  Y <- pull(obs, Y)
  A <- pull(obs, A)
  psi_n <- mean(A * Y) / mean(A) - mean((1 - A) * Y) / mean(1 - A)
  Var_n <- cov(cbind(A * Y, A, (1 - A) * Y, (1 - A)))
  phi_n <- c(1 / mean(A), -mean(A * Y) / mean(A)^2,
             -1 / mean(1 - A),
             mean((1 - A) * Y) / mean(1 - A)^2)
  var_n <- as.numeric(t(phi_n) %*% Var_n %*% phi_n)
  sig_n <- sqrt(var_n / nrow(obs))
  tibble(psi_n = psi_n, sig_n = sig_n)
}

## ----known-Gbar-one-a----------------------------------------------------
Gbar <- get_feature(experiment, "Gbar")

## ----known-Gbar-one-b, fig.cap = '(ref:known-Gbar-one-b)'----------------
psi_hat_ab <- obs %>% as_tibble() %>%
  mutate(id = (seq_len(n()) - 1) %% iter) %>%
  nest(-id, .key = "obs") %>%
  mutate(est_a = map(obs, ~ compute_irrelevant_estimator(.)),
         est_b = map(obs, ~ compute_iptw(as.matrix(.), Gbar))) %>%
  gather(`est_a`, `est_b`, key = "type", value = "estimates") %>%
  extract(type, "type", "_([ab])$") %>%
  unnest(estimates) %>% select(-obs)

(psi_hat_ab)

psi_hat_ab <- psi_hat_ab %>% 
  group_by(id) %>%
  mutate(clt = (psi_n - psi_zero) / sig_n)

(psi_hat_ab)

(bias_ab <- psi_hat_ab %>%
   group_by(type) %>% summarise(bias = mean(clt)))

fig_bias_ab <- ggplot() +
  geom_line(aes(x = x, y = y), 
            data = tibble(x = seq(-3, 3, length.out = 1e3),
                          y = dnorm(x)),
            linetype = 1, alpha = 0.5) +
  geom_density(aes(clt, fill = type, colour = type),
               psi_hat_ab, alpha = 0.1) +
  geom_vline(aes(xintercept = bias, colour = type),
             bias_ab, size = 1.5, alpha = 0.5)
  
fig_bias_ab +
  labs(y = "",
       x = expression(paste(sqrt(n/v[n]^{list(a, b)})*
                            (psi[n]^{list(a, b)} - psi[0]))))

## ----estimate-QW-one-----------------------------------------------------
QW_hat <- estimate_QW(head(obs, 1e3))

## ----estimate-QW-two, fig.cap = '(ref:estimate-QW-two)'------------------
empirical_experiment <- LAW()
alter(empirical_experiment, QW = QW_hat)
alter(empirical_experiment, sample_from = function(n) {
  QW <- get_feature(empirical_experiment, "QW")
  W <- sample(pull(QW, "value"), n, prob = pull(QW, "weight"))
  cbind(W = W, A = NA, Y = NA)
})
W <- sample_from(empirical_experiment, 1e3) %>% as_tibble
W %>% ggplot() +
  geom_histogram(aes(x = W, y = stat(density)), bins = 40) +
  stat_function(fun = get_feature(experiment, "QW"), col = "red")

## ----estimate-QW-three---------------------------------------------------
(length(intersect(pull(W, W), head(obs[, "W"], 1e3))))

## ----estimate-Gbar-one---------------------------------------------------
working_model_G_one

## ----estimate-Gbar-two---------------------------------------------------
Gbar_hat <- estimate_Gbar(head(obs, 1e3), algorithm = working_model_G_one)

## ----estimate-Gbar-three, fig.cap = '(ref:estimate-Gbar-three)'----------
tibble(w = seq(0, 1, length.out = 1e3)) %>%
  mutate("truth" = Gbar(w),
         "estimated" = compute_Gbar_hatW(w, Gbar_hat)) %>%
  gather("f", "value", -w) %>%
  ggplot() + geom_line(aes(x = w, y = value, color = f), size = 1) +
  labs(y = "f(w)",
       title = bquote("Visualizing" ~ bar(G)[0] ~ "and" ~ hat(G)[n])) +
  ylim(NA, 1)

## ----estimate-Qbar-one---------------------------------------------------
working_model_Q_one

## ----estimate-Qbar-two---------------------------------------------------
Qbar_hat_kknn <- estimate_Qbar(head(obs, 1e3),
                               algorithm = kknn_algo,
                               trControl = kknn_control,
                               tuneGrid = kknn_grid)

## ----estimate-Qbar-three-------------------------------------------------
fig <- tibble(w = seq(0, 1, length.out = 1e3),
              truth_1 = Qbar(cbind(A = 1, W = w)),
              truth_0 = Qbar(cbind(A = 0, W = w)),
              kNN_1 = compute_Qbar_hatAW(1, w, Qbar_hat_kknn),
              kNN_0 = compute_Qbar_hatAW(0, w, Qbar_hat_kknn))

## ----estimate-Qbar-four--------------------------------------------------
Qbar_hat_trees <- estimate_Qbar(head(obs, 1e3),
                                algorithm = bstTree_algo,
                                trControl = bstTree_control,
                                tuneGrid = bstTree_grid)

Qbar_hat_trees %>% dplyr::filter(a == "one") %>% pull(fit) %>%
  capture.output %>% tail(3) %>% str_wrap(width = 60) %>% cat
                                                             
Qbar_hat_trees %>% dplyr::filter(a == "zero") %>% pull(fit) %>%
  capture.output %>% tail(3) %>% str_wrap(width = 60) %>% cat

## ----estimate-Qbar-five, fig.cap = '(ref:estimate-Qbar-five)'------------
fig %>%
  mutate(trees_1 = compute_Qbar_hatAW(1, w, Qbar_hat_trees),
         trees_0 = compute_Qbar_hatAW(0, w, Qbar_hat_trees)) %>%
  gather("f", "value", -w) %>%
  extract(f, c("f", "a"), "([^_]+)_([01]+)") %>%
  mutate(a = paste0("a=", a)) %>%
  ggplot +
  geom_line(aes(x = w, y = value, color = f), size = 1) +
  labs(y = "f(w)",
       title = bquote("Visualizing" ~ bar(Q)[0] ~ "and its estimators")) +
  ylim(NA, 1) +
  facet_wrap(~ a)

## ----psi-n-b-recall-one, eval = FALSE------------------------------------
## compute_iptw(head(obs, 1e3), Gbar)

## ----psi-n-b-recall-two--------------------------------------------------
Gbar_hat <- estimate_Gbar(head(obs, 1e3), working_model_G_one)
compute_iptw(head(obs, 1e3), wrapper(Gbar_hat)) %>% pull(psi_n)

## ----unknown-Gbar-two-bis------------------------------------------------
learned_features_fixed_sample_size <-
  obs %>% as_tibble() %>%
  mutate(id = (seq_len(n()) - 1) %% iter) %>%
  nest(-id, .key = "obs") %>%
  mutate(Gbar_hat =
           map(obs,
               ~ estimate_Gbar(., algorithm = working_model_G_one)))

psi_hat_abc <-
  learned_features_fixed_sample_size %>%
  mutate(est_c =
           map2(obs, Gbar_hat,
                ~ compute_iptw(as.matrix(.x), wrapper(.y, FALSE)))) %>%
  unnest(est_c) %>% select(-Gbar_hat, -obs) %>%
  mutate(clt = (psi_n - psi_zero) / sig_n,
         type = "c") %>%
  full_join(psi_hat_ab)

(bias_abc <- psi_hat_abc %>%
   group_by(type) %>% summarise(bias = mean(clt)))

## ----unknown-Gbar-three, fig.cap = '(ref:unknown-Gbar-three)'------------
fig_bias_ab +
  geom_density(aes(clt, fill = type, colour = type), psi_hat_abc, alpha = 0.1) +
  geom_vline(aes(xintercept = bias, colour = type),
             bias_abc, size = 1.5, alpha = 0.5) +
  xlim(-3, 4) + 
  labs(y = "",
       x = expression(paste(sqrt(n/v[n]^{list(a, b, c)})*
                            (psi[n]^{list(a, b, c)} - psi[0]))))

## ----unknown-Gbar-four, fig.cap = '(ref:unknown-Gbar-four)'--------------
ggplot(psi_hat_abc, aes(sample = clt, fill = type, colour = type)) +
  geom_abline(intercept = 0, slope = 1, alpha = 0.5) +
  geom_qq(alpha = 1)

## ----unknow-Gbar-five----------------------------------------------------
(emp_sig_n <- psi_hat_abc %>% filter(type == "c") %>%
   summarize(sd(psi_n)) %>% pull)
(summ_sig_n <- psi_hat_abc %>% filter(type == "c") %>% select(sig_n) %>%
   summary)

## ----unknown-Gbar-six, echo = FALSE--------------------------------------
mean_sig_n <- psi_hat_abc %>% filter(type == "c") %>%
  select(sig_n) %>% pull %>% mean

## ----unknown-Gbar-seven, fig.cap = '(ref:unknown-Gbar-seven)'------------
clt_c <- psi_hat_abc %>% filter(type == "c") %>%
  mutate(clt = clt * sig_n /  emp_sig_n)

fig_bias_ab +
  geom_density(aes(clt, fill = type, colour = type), clt_c, alpha = 0.1) +
  geom_vline(aes(xintercept = bias, colour = type),
             bias_abc, size = 1.5, alpha = 0.5) +
  xlim(-3, 4) + 
  labs(y = "",
       x = expression(paste(sqrt(n/v[n]^{list(a, b, c)})*
                            (psi[n]^{list(a, b, c)} - psi[0]))))

## ----working-model-G-two, eval = FALSE-----------------------------------
## ## make sure '1/2' and '1' belong to 'powers'
## powers <- rep(seq(1/4, 3, by = 1/4), each = 2)
## working_model_G_two <- list(
##   model = function(...) {trim_glm_fit(glm(family = binomial(), ...))},
##   formula = stats::as.formula(
##     paste("A ~",
##           paste(c("I(W^", "I(abs(W - 5/12)^"),
##                 powers,
##                 sep = "", collapse = ") + "),
##           ")")
##   ),
##   type_of_preds = "response"
## )
## attr(working_model_G_two, "ML") <- FALSE

## ----estimate-QW-one-redo, eval = FALSE----------------------------------
## QW_hat <- estimate_QW(head(obs, 1e3))

## ----psi-n-de-redo, eval = FALSE-----------------------------------------
## Qbar_hat_kknn <- estimate_Qbar(head(obs, 1e3),
##                                algorithm = kknn_algo,
##                                trControl = kknn_control,
##                                tuneGrid = kknn_grid)

## ----psi-n-de-one--------------------------------------------------------
Qbar_hat_d <- estimate_Qbar(head(obs, 1e3), working_model_Q_one)

## ----psi-n-de-two--------------------------------------------------------
(compute_gcomp(QW_hat, wrapper(Qbar_hat_kknn, FALSE), 1e3))
(compute_gcomp(QW_hat, wrapper(Qbar_hat_d, FALSE), 1e3))

## ----estimating-Qbar-one-bis, fig.cap = '(ref:estimating-Qbar-one-bis)'----
learned_features_fixed_sample_size <-
  learned_features_fixed_sample_size %>% 
  mutate(Qbar_hat_d =
           map(obs,
               ~ estimate_Qbar(., algorithm = working_model_Q_one)),
         Qbar_hat_e =
           map(obs,
               ~ estimate_Qbar(., algorithm = kknn_algo,
                               trControl = kknn_control,
                               tuneGrid = kknn_grid))) %>%
  mutate(QW = map(obs, estimate_QW),
         est_d =
           pmap(list(QW, Qbar_hat_d, n()),
                ~ compute_gcomp(..1, wrapper(..2, FALSE), ..3)),
         est_e =
           pmap(list(QW, Qbar_hat_e, n()),
                ~ compute_gcomp(..1, wrapper(..2, FALSE), ..3)))

psi_hat_de <- learned_features_fixed_sample_size %>%
  select(est_d, est_e) %>%
  gather(`est_d`, `est_e`, key = "type", value = "estimates") %>%
  extract(type, "type", "_([de])$") %>% 
  unnest(estimates) %>%
  group_by(type) %>%
  mutate(sig_alt = sd(psi_n)) %>%
  mutate(clt_ = (psi_n - psi_zero) / sig_n,
         clt_alt = (psi_n - psi_zero) / sig_alt) %>%
  gather(`clt_`, `clt_alt`, key = "key", value = "clt") %>%
  extract(key, "key", "_(.*)$") %>%
  mutate(key = ifelse(key == "", TRUE, FALSE)) %>%
  rename("auto_renormalization" = key)

(bias_de <- psi_hat_de %>%
   group_by(type, auto_renormalization) %>%
   summarize(bias = mean(clt)) %>% ungroup)

fig <- ggplot() +
  geom_line(aes(x = x, y = y), 
            data = tibble(x = seq(-4, 4, length.out = 1e3),
                          y = dnorm(x)),
            linetype = 1, alpha = 0.5) +
  geom_density(aes(clt, fill = type, colour = type),
               psi_hat_de, alpha = 0.1) +
  geom_vline(aes(xintercept = bias, colour = type),
             bias_de, size = 1.5, alpha = 0.5) +
  facet_wrap(~ auto_renormalization,
             labeller =
               as_labeller(c(`TRUE` = "auto-renormalization: TRUE",
                             `FALSE` = "auto-renormalization: FALSE")),
             scales = "free")
  
fig +
  labs(y = "",
       x = expression(paste(sqrt(n/v[n]^{list(d, e)})*
                            (psi[n]^{list(d, e)} - psi[0]))))

## ----bias-de, echo = FALSE-----------------------------------------------
bias_d_T <- bias_de %>%
  filter(type == "d" & auto_renormalization) %>%
  pull(bias) %>% round(digits = 3)
bias_d_F <- bias_de %>%
  filter(type == "d" & !auto_renormalization) %>%
  pull(bias) %>% round(digits = 3)
bias_e_T <- bias_de %>%
  filter(type == "e" & auto_renormalization) %>%
  pull(bias) %>% round(digits = 3)
bias_e_F <- bias_de %>%
  filter(type == "e" & !auto_renormalization) %>%
  pull(bias) %>% round(digits = 3)

## ----var-de--------------------------------------------------------------
## psi_n^d
(psi_hat_de %>% ungroup %>%
   filter(type == "d" & auto_renormalization) %>% pull(sig_n) %>% summary)
## psi_n^e
(psi_hat_de %>% ungroup %>%
   filter(type == "e" & auto_renormalization) %>% pull(sig_n) %>% summary)

## ----echo = FALSE--------------------------------------------------------
emp_sig_n_d <- psi_hat_de %>% ungroup %>%
   filter(type == "d" & auto_renormalization) %>% pull(sig_n) %>% mean
emp_sig_n_e <- psi_hat_de %>% ungroup %>%
  filter(type == "e" & auto_renormalization) %>% pull(sig_n) %>% mean

mean_sig_n_d <- psi_hat_de %>% ungroup %>%
  filter(type == "d" & !auto_renormalization) %>% select(sig_alt) %>% pull %>% mean ## it is constant
mean_sig_n_e <- psi_hat_de %>% ungroup %>%
  filter(type == "e" & !auto_renormalization) %>% select(sig_alt) %>% pull %>% mean ## it is constant

## ----estimating-Qbar-two, eval = TRUE------------------------------------
sample_size <- c(5e3, 15e3)
block_size <- sum(sample_size)


learned_features_varying_sample_size <- obs %>% as_tibble %>% 
  head(n = (nrow(.) %/% block_size) * block_size) %>% 
  mutate(block = label(1:nrow(.), sample_size)) %>%
  nest(-block, .key = "obs")

## ----estimating-Qbar-three-----------------------------------------------
learned_features_varying_sample_size <-
  learned_features_varying_sample_size %>%
  mutate(Qbar_hat_d =
           map(obs,
               ~ estimate_Qbar(., algorithm = working_model_Q_one)),
         Qbar_hat_e =
           map(obs,
               ~ estimate_Qbar(., algorithm = kknn_algo,
                               trControl = kknn_control,
                               tuneGrid = kknn_grid))) %>%
  mutate(QW = map(obs, estimate_QW),
         est_d =
           pmap(list(QW, Qbar_hat_d, n()),
                ~ compute_gcomp(..1, wrapper(..2, FALSE), ..3)),
         est_e =
           pmap(list(QW, Qbar_hat_e, n()),
                ~ compute_gcomp(..1, wrapper(..2, FALSE), ..3)))

root_n_bias <- learned_features_varying_sample_size %>%
  mutate(block = unlist(map(strsplit(block, "_"), ~.x[2])),
         sample_size = sample_size[as.integer(block)]) %>%
  select(block, sample_size, est_d, est_e) %>%
  gather(`est_d`, `est_e`, key = "type", value = "estimates") %>%
  extract(type, "type", "_([de])$") %>% 
  unnest(estimates) %>%
  group_by(block, type) %>%
  mutate(sig_alt = sd(psi_n)) %>%
  mutate(clt_ = (psi_n - psi_zero) / sig_n,
         clt_alt = (psi_n - psi_zero) / sig_alt) %>%
  gather(`clt_`, `clt_alt`, key = "key", value = "clt") %>%
  extract(key, "key", "_(.*)$") %>%
  mutate(key = ifelse(key == "", TRUE, FALSE)) %>%
  rename("auto_renormalization" = key)

## ----estimating-Qbar-four, fig.width = 5, fig.height = 5, fig.cap = '(ref:estimating-Qbar-four)'----
root_n_bias <- learned_features_fixed_sample_size %>%
   mutate(block = "0",
          sample_size = B/iter) %>%  # because *fixed* sample size
   select(block, sample_size, est_d, est_e) %>%
   gather(`est_d`, `est_e`, key = "type", value = "estimates") %>%
   extract(type, "type", "_([de])$") %>% 
   unnest(estimates) %>%
   group_by(block, type) %>%
   mutate(sig_alt = sd(psi_n)) %>%
   mutate(clt_ = (psi_n - psi_zero) / sig_n,
          clt_alt = (psi_n - psi_zero) / sig_alt) %>%
   gather(`clt_`, `clt_alt`, key = "key", value = "clt") %>%
   extract(key, "key", "_(.*)$") %>%
   mutate(key = ifelse(key == "", TRUE, FALSE)) %>%
   rename("auto_renormalization" = key) %>%
   full_join(root_n_bias)
 
root_n_bias %>% filter(auto_renormalization) %>%
  mutate(rnb = sqrt(sample_size) * (psi_n - psi_zero)) %>%
  group_by(sample_size, type) %>%
  ggplot() +
  stat_summary(aes(x = sample_size, y = rnb,
                   group = interaction(sample_size, type),
                   color = type),
               fun.data = mean_se, fun.args = list(mult = 1),
               position = position_dodge(width = 250), cex = 1) +
  stat_summary(aes(x = sample_size, y = rnb,
                   group = interaction(sample_size, type),
                   color = type),
               fun.data = mean_se, fun.args = list(mult = 1),
               position = position_dodge(width = 250), cex = 1,
               geom = "errorbar", width = 750) +
  stat_summary(aes(x = sample_size, y = rnb,
                   color = type),
               fun.y = mean,
               position = position_dodge(width = 250),
               geom = "polygon", fill = NA) +
  geom_point(aes(x = sample_size, y = rnb,
                 group = interaction(sample_size, type),
                 color = type),
             position = position_dodge(width = 250),
             alpha = 0.1) +
  scale_x_continuous(breaks = unique(c(B / iter, sample_size))) +
  labs(x = "sample size n",
       y = expression(paste(sqrt(n) * (psi[n]^{list(d, e)} - psi[0]))))

## ----psi-n-de-two-bis----------------------------------------------------
(psin_kknn <- compute_gcomp(QW_hat, wrapper(Qbar_hat_kknn, FALSE), 1e3))
(psin_kknn_os <- apply_one_step_correction(head(obs, 1e3),
                                           wrapper(Gbar_hat, FALSE),
                                           wrapper(Qbar_hat_kknn, FALSE),
                                           psin_kknn$psi_n)) 

## ----one-step-one, fig.cap = '(ref:one-step-one)'------------------------
psi_hat_de_one_step <- learned_features_fixed_sample_size %>%
  mutate(os_est_d =
           pmap(list(obs, Gbar_hat, Qbar_hat_d, est_d),
                ~ apply_one_step_correction(as.matrix(..1),
                                 wrapper(..2, FALSE),
                                 wrapper(..3, FALSE),
                                 ..4$psi_n)),
         os_est_e =
           pmap(list(obs, Gbar_hat, Qbar_hat_e, est_e),
                ~ apply_one_step_correction(as.matrix(..1),
                                 wrapper(..2, FALSE),
                                 wrapper(..3, FALSE),
                                 ..4$psi_n))) %>%
  select(os_est_d, os_est_e) %>%
  gather(`os_est_d`, `os_est_e`, key = "type", value = "estimates") %>%
  extract(type, "type", "_([de])$") %>%
  mutate(type = paste0(type, "_one_step")) %>%
  unnest(estimates) %>%
  group_by(type) %>%
  mutate(sig_alt = sd(psi_n)) %>%
  mutate(clt_ = (psi_n - psi_zero) / sig_n,
         clt_alt = (psi_n - psi_zero) / sig_alt) %>%
  gather(`clt_`, `clt_alt`, key = "key", value = "clt") %>%
  extract(key, "key", "_(.*)$") %>%
  mutate(key = ifelse(key == "", TRUE, FALSE)) %>%
  rename("auto_renormalization" = key)
  
(bias_de_one_step <- psi_hat_de_one_step %>%
   group_by(type, auto_renormalization) %>%
   summarize(bias = mean(clt)) %>% ungroup)

fig <- ggplot() +
  geom_line(aes(x = x, y = y), 
            data = tibble(x = seq(-4, 4, length.out = 1e3),
                          y = dnorm(x)),
            linetype = 1, alpha = 0.5) +
  geom_density(aes(clt, fill = type, colour = type),
               psi_hat_de_one_step, alpha = 0.1) +
  geom_vline(aes(xintercept = bias, colour = type),
             bias_de_one_step, size = 1.5, alpha = 0.5) +
  facet_wrap(~ auto_renormalization,
             labeller =
               as_labeller(c(`TRUE` = "auto-renormalization: TRUE",
                             `FALSE` = "auto-renormalization: FALSE")),
             scales = "free")
  
fig +
  labs(y = "",
       x = expression(paste(sqrt(n/v[n]^{list(d, e, os)})*
                            (psi[n]^{list(d, e, os)} - psi[0]))))

## ----one-step-two--------------------------------------------------------
bind_rows(bias_de, bias_de_one_step) %>% filter(!auto_renormalization)

## ----one-step-three------------------------------------------------------
psi_hat_de %>%
  full_join(psi_hat_de_one_step) %>%
  filter(auto_renormalization) %>% 
  group_by(type) %>%
  summarize(sd = mean(sig_n),
            se = sd(psi_n),
            mse = mean((psi_n - psi_zero)^2) * n())

## ----show-first-piece-solved---------------------------------------------
# maybe would be helpful to include a bit of code here
# for evaluating EIF using empirical dist of W and showing that
# indeed the second bit is solved? Not sure. The algebra is simple enough.
# I'm just trying to break up all the writing a bit in this section, because
# it is quite dense. 

## ----plot-for-several-epsilon--------------------------------------------
# can include the graph of Qbar_n,eps(a,W) ~ W for eps = -1, 0, 1 \times a = 0,1?
# would like to drive home the point that each epsilon is simply a different regresion
# fit.

# could also show for different Gbar_n showing that the submodel depends on Gbar_n, 
# but I actually think that's far less important. 

## ----compute-negloglik-and-plot------------------------------------------
# basically just do a grid search over epsilon for minimizer of negative log-likelihood loss

## ----show-negloglik-as-function-of-delta---------------------------------
# we could also show this earlier and comment about how it
# should be the same as the plot of neg log lik as a fn of epsilon
# just shifted over. Maybe that would help drive home the point that
# the submodels are the same?

## ----tmle, fig.cap = '(ref:tmle)'----------------------------------------
psi_tmle <- learned_features_fixed_sample_size %>%
  mutate(tmle_d =
           pmap(list(obs, Gbar_hat, Qbar_hat_d),
                ~ apply_targeting_step(as.matrix(..1),
                                 wrapper(..2, FALSE),
                                 wrapper(..3, FALSE))),
         tmle_e =
           pmap(list(obs, Gbar_hat, Qbar_hat_e),
                ~ apply_targeting_step(as.matrix(..1),
                                 wrapper(..2, FALSE),
                                 wrapper(..3, FALSE)))) %>%
  select(tmle_d, tmle_e) %>%
  gather(`tmle_d`, `tmle_e`, key = "type", value = "estimates") %>%
  extract(type, "type", "_([de])$") %>%
  mutate(type = paste0(type, "_targeted")) %>%
  unnest(estimates) %>%
  group_by(type) %>%
  mutate(sig_alt = sd(psi_n)) %>%
  mutate(clt_ = (psi_n - psi_zero) / sig_n,
         clt_alt = (psi_n - psi_zero) / sig_alt) %>%
  gather(`clt_`, `clt_alt`, key = "key", value = "clt") %>%
  extract(key, "key", "_(.*)$") %>%
  mutate(key = ifelse(key == "", TRUE, FALSE)) %>%
  rename("auto_renormalization" = key)

(bias_tmle <- psi_tmle %>%
   group_by(type, auto_renormalization) %>%
   summarize(bias = mean(clt)) %>% ungroup)

fig <- ggplot() +
  geom_line(aes(x = x, y = y), 
            data = tibble(x = seq(-4, 4, length.out = 1e3),
                          y = dnorm(x)),
            linetype = 1, alpha = 0.5) +
  geom_density(aes(clt, fill = type, colour = type),
               psi_tmle, alpha = 0.1) +
  geom_vline(aes(xintercept = bias, colour = type),
             bias_tmle, size = 1.5, alpha = 0.5) +
  facet_wrap(~ auto_renormalization,
             labeller =
               as_labeller(c(`TRUE` = "auto-renormalization: TRUE",
                             `FALSE` = "auto-renormalization: FALSE")),
             scales = "free")
  
fig +
  labs(y = "",
       x = expression(paste(sqrt(n/v[n]^{"*"})*
                            (psi[n]^{"*"} - psi[0]))))


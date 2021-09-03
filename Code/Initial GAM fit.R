pdat <- expand.grid(DAY = seq(1, 6, length = 400),
                    GROUP = c('CG', 'MG', 'MTD'))




smooth_diff <- function(model, newdata, f1, f2, var, alpha = 0.05,
                        unconditional = FALSE) {
  xp <- predict(model, newdata = newdata, type = 'lpmatrix')
  c1 <- grepl(f1, colnames(xp))
  c2 <- grepl(f2, colnames(xp))
  r1 <- newdata[[var]] == f1
  r2 <- newdata[[var]] == f2
  ## difference rows of xp for data from comparison
  X <- xp[r1, ] - xp[r2, ]
  ## zero out cols of X related to splines for other lochs
  X[, ! (c1 | c2)] <- 0
  ## zero out the parametric cols
  X[, !grepl('^s\\(', colnames(xp))] <- 0
  dif <- X %*% coef(model)
  se <- sqrt(rowSums((X %*% vcov(model, unconditional = unconditional)) * X))
  crit <- qt(alpha/2, df.residual(model), lower.tail = FALSE)
  upr <- dif + (crit * se)
  lwr <- dif - (crit * se)
  data.frame(pair = paste(f1, f2, sep = '-'),
             diff = dif,
             se = se,
             upper = upr,
             lower = lwr)
}

comp1 <- smooth_diff(test, pdat, 'CG', 'MG', 'GROUP')
comp2 <- smooth_diff(test, pdat, 'CG', 'MTD', 'GROUP')
comp3 <- smooth_diff(test, pdat, 'MG', 'MTD', 'GROUP')
comp <- cbind(DAY = seq(1, 6, length = 400),
              rbind(comp1, comp2, comp3))

ggplot(comp, aes(x = DAY, y = diff, group = pair)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_line() +
  facet_wrap(~ pair, ncol = 2) +
  coord_cartesian(ylim = c(-30,30)) +
  labs(x = NULL, y = 'Difference in Hg trend')



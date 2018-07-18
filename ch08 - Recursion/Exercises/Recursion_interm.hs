module Recursion_interm where

-- Write out the evaluation of the following. It might be a little less noisy
-- if you do so with the form that didn’t use (.).


-- applyTimes 5 (+1) 5
-- (+1) . applyTimes(4) (+1) $ 5
-- (+1) . (+1) . applyTimes(3) (+1) $5
-- (+1) . (+1) . (+1) . applyTimes(2) (+1) $5
-- (+1) . (+1) . (+1) . (+1) . applyTimes(1) (+1) $5
-- (+1) . (+1) . (+1) . (+1) . (+1) . applyTimes(0) (+1) $5
-- 10

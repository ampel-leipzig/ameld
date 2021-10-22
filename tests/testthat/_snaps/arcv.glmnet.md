# arcv.glmnet

    Code
      print(arcv)
    Output
      
      Call: arcv.glmnet(x = x, y = y, alpha = c(0, 1), nrepcv = 2, nfolds = 3,      trace.it = FALSE) 
      
      Models: 2
      Alpha: 0 1
      Number of CV for Lambda: 3
      Number of repeated CV for Lambda: 2
      
      
      Measure: Mean-Squared Error 
      
      Lambda min:
           Alpha Lambda Index Measure     SE Nonzero
      [1,]     0  2.410    72   26.42 1.0246     100
      [2,]     1  0.191    25   24.81 0.8838      21
      
      Lambda 1se:
           Alpha Lambda Index Measure     SE Nonzero
      [1,]     0  8.864    58   27.40 1.0428     100
      [2,]     1  0.402    17   25.52 0.9084      10


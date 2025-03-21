# 命名方式

1. **變數名稱應該要能充分的說明自身**

   **範例📌**

   - `x <- 243  # 不好，名稱過於模糊`
   - `total_pay_lines <- 243  # 較好，能清楚表達變數用途`

2. **主要使用全小寫的蛇形命名 (snake_case)，只有固定數值才使用全大寫(SNAKE_CASE)**

   **範例📌**

   - `EULER_CONSTANT <- 0.57721566490153286060651209`
   - `DAYS_IN_LEAP_YEAR <- 366`

3. **命名包含其變數的形態或用途**  
   - 表示資料形式：  
     - `data.frame` 用 `df` 結尾  
     - `data.table` 用 `tb` 結尾  
     - 矩陣用 `mat` 結尾  
     - 陣列用 `arr`  
     - 邏輯值用疑問句開頭  
     例如：`sales_df`、`cov_mat`、`is_valid`
   - 表示變數用途：  
     - 有歧義的迴圈變數使用 `_ind` 結尾，apply() 等函數則不限制  
       例如：`for (k_ind in 1:M){…}`、`sapply(1:5, function(x) x^2)`
     - 在程式初始化就設定好的固定數值（後續程式不會變化者），使用大寫表示  
       例如：`RTP = 0.9`

4. **副檔名使用 `.R`，不要用 `.r`**

5. **在算式複雜時，可以使用簡單符號的方式來代替部分計算的過程，使用變數後面加底線來表示暫時計算的結果**

   **範例📌**

   計算 $ax^2+bx+c = 0$ 的兩根 (假設已知 $\Delta = b^2-4ac > 0$ )
   ，但是更推薦使用 `delta` 為 `d_` 命名，更複雜的算式請見 `6.` 的範例
   ```r
   # Solve x^2 + 2x - 3 = 0
   a <- 1
   b <- 2
   const <- -3
   d_ <- b^2 - 4 * a * const
   solution <- c((-b + sqrt(d_)) / (2 * a), (-b - sqrt(d_)) / (2 * a))
   ```
6. **複雜的公式可以使用多個變量來計算**

   **範例📌**
   
   計算介於 `0 ~ 1` 之間參數的logit轉換後信賴區間

    <!-- $$
    \left[
    \frac{e^{\log \frac{\hat{p}}{1-\hat{p}} - z_{\alpha/2} \sqrt{\frac{1}{n\hat{p}(1-\hat{p})}}}}
    {1 + e^{\log \frac{\hat{p}}{1-\hat{p}} - z_{\alpha/2} \sqrt{\frac{1}{n\hat{p}(1-\hat{p})}}}},
    \quad
    \frac{e^{\log \frac{\hat{p}}{1-\hat{p}} + z_{\alpha/2} \sqrt{\frac{1}{n\hat{p}(1-\hat{p})}}}}
    {1 + e^{\log \frac{\hat{p}}{1-\hat{p}} + z_{\alpha/2} \sqrt{\frac{1}{n\hat{p}(1-\hat{p})}}}}
    \right]
    $$ -->

   ![logit transformed CI](pics/logit-transformed-CI.svg)

   ```r
   # 計算 Logit 信賴區間
   logit_ci <- function(k, n, conf.level = 0.95) {
     # 檢查數據合理性
     if (k < 0 || n <= 0 || k > n) { stop("k 必須在 0 到 n 之間") }
     
     # 估計成功機率 p_hat
     p_hat <- k / n
     
     # 針對 p_hat = 0 或 1 進行調整，避免 log(0) 問題
    if (p_hat == 0) { p_hat <- 1 / (2 * n) }
    if (p_hat == 1) { p_hat <- 1 - 1 / (2 * n) }
     
     # 計算 logit 值
     logit_p <- log(p_hat / (1 - p_hat))
     
     # 標準誤 (SE) 計算
     se_logit <- sqrt(1 / (n * p_hat * (1 - p_hat)))
     
     # 標準常態分布的 z 值
     alpha <- 1 - conf.level
     z_value <- qnorm(1 - alpha / 2)
     
     # 計算 logit 空間的信賴區間
     logit_low <- logit_p - z_value * se_logit
     logit_high <- logit_p + z_value * se_logit
     
     # 轉換回機率空間
     p_low <- exp(logit_low) / (1 + exp(logit_low))
     p_high <- exp(logit_high) / (1 + exp(logit_high))
     
     # 返回結果
     return(c(lower = p_low, upper = p_high))
   }
   
   # 測試
   logit_ci(k = 30, n = 100)
   logit_ci(k = 5, n = 10)
   logit_ci(k = 1, n = 10)
   
   ```
# 撰寫風格

1. **變數指派使用 `<-`** (alt鍵+”-”快速生成)，函數指派參數才使用 `=`
2. **if、for、while 等迴圈**  
   - 縮排 4 格或使用 tab（推薦縮排 4 格）
   - 一律使用大括號 `{}` 包裹程式，多行程式時 `}` 必須與開頭對齊

   **正確範例📌**

   - 一行程式：
     ```r
     for (i in 1:M) { <一行程式> }
     ```
   - 多行程式：
     ```r
     for (i_ind in 1:M) { 
         <多行程式> 
     }
     ```
   - if 條件判斷：
     ```r
     if (<條件一>) {
         <程式一>
     } else if (<條件二>) {
         <程式二>
     } else {
         <程式三>
     }
     ```

   **❌錯誤範例**

   - ❌不含 `{}`：
     ```r
     for (i_ind in 1:M) <一行程式>  # ❌不含大括號 {}
     ```
   - ❌多行迴圈時首行不輸入程式內容，且 `}` 未對齊：
     ```r
     for (i_ind in 1:M) { <第一行程式> # ❌多行迴圈時首行輸入程式內容 
     <第二行程式>
           }  # ❌ } 未對齊
     ```

3. **註解**  
   - 使用 `#`，且 `#` 後固定有至少 1 個空格

4. **運算子** (如: `+`, `-`, `*`, `/`, `||`, `&&`, `%%`, `%/%`)  
   - 左右要有空格 (但 `:` 和 `$` 和 `^` 除外)
   - 逗號 `,` 後要有空格

# 程式註解

1. **開頭進行程式的基本說明**  
   內容至少包括程式名稱、程式用途（為什麼要寫這個程式）、所屬專案、程式架構，這部分越詳細越好

   **範例📌**

   ```r
   # ======================================
   # Project Name: FQ-sim
   # Script Purpose: 以模擬的方式，確認機率試算表的正確性
   # background： 首次獨立開發新老虎機遊戲
   # project: K8
   # Author: 王大明
   # Created Date: 2024/12/31
   # 程式架構：
   #   1. 建立賠率表
   #   2. 線數設定、滾輪設定
   #   3. 判定贏分函數
   #   4. 免費遊戲判定函數
   #   5. 模擬主要迴圈部分
   # ======================================
   ```
2. **函數的簡短說明**
函數的前面說明輸入、函數功能、輸出

   **範例📌**

   ```r
   # 輸入：滾輪、賠率表、某一條連線
   # 功能：老虎機單線、單次遊戲進行與分數計算
   # 輸出：單線的盤面、連線結果 (ex: J4 連)、獎金金額
   judge <- function(roll, index, pay_table, line = rep(0, 5)) { # 判斷單線得分的函數
       mylist <- list()
       show <- rep(NA, 5)
       for (i_ind in 1:5) {
           index[i_ind] <- (index[i_ind] + line[i_ind] - 1) %% length(roll[[i_ind]]) + 1  # index 表示轉到滾輪哪一個位置，line 代表考慮特定某一條線的情況
           show[i_ind] <- c(roll[[i_ind]][index[i_ind]])
       }
       
       mylist$show <- paste0(show[1], show[2], show[3], show[4], show[5])  # show 為本次轉到的五個符號
       
       if (show[1] != "WW") {
           symb <- show[1]
       } else if (show[2] != "WW") {
           symb <- show[2]  # 從第一個按照順序開始檢查，第一個非 WW 的符號即為這條線的中獎符號
       } else if (show[3] != "WW") {
           symb <- show[3]
       } else if (show[4] != "WW") {
           symb <- show[4]
       } else if (show[5] != "WW") {
           symb <- show[5]
       } else {
           symb <- "WW"
       }
       
       hit <- 1  # hit 用於判斷中獎的連線數
       if (show[2] == symb || show[2] == "WW") {
           hit <- hit + 1
       }
       if ((show[3] == symb || show[3] == "WW") && hit == 2) {
           hit <- hit + 1
       }
       if ((show[4] == symb || show[4] == "WW") && hit == 3) {
           hit <- hit + 1
       }
       if ((show[5] == symb || show[5] == "WW") && hit == 4) {
           hit <- hit + 1
       }
       mylist$lines <- paste0(symb, "+", hit)
       mylist$pay <- pay_table[symb, hit]  # 從賠率表中獲取贏分
       return(mylist)
   }
   
   ```



// 标准 MoonBit 测试增强用例
// 使用内置运算符和标准 MoonBit 测试语法

test "addition_commutative_property" {
  // 测试加法交换律
  let a = 15
  let b = 27
  let result1 = a + b
  let result2 = b + a
  if (result1 != result2) {
    @builtin.abort("Addition commutative property test failed")
  }
  
  // 测试负数交换律
  let x = -12
  let y = 8
  let result3 = x + y
  let result4 = y + x
  if (result3 != result4) {
    @builtin.abort("Negative addition commutative property test failed")
  }
}

test "multiplication_distributive_property" {
  // 测试乘法分配律 a * (b + c) = a * b + a * c
  let a = 4
  let b = 7
  let c = 9
  
  let left_side = a * (b + c)
  let right_side = a * b + a * c
  if (left_side != right_side) {
    @builtin.abort("Multiplication distributive property test failed")
  }
}

test "integer_division_ceil_function" {
  // 测试向上取整除法函数
  let divide_with_ceil = fn(a : Int, b : Int) -> Int {
    if b == 0 { 
      0 
    } else {
      let quotient = a / b
      let remainder = a % b
      if remainder == 0 {
        quotient
      } else if a > 0 && b > 0 {
        quotient + 1
      } else if a < 0 && b < 0 {
        quotient + 1
      } else {
        quotient
      }
    }
  }
  
  // 正数小数
  let result1 = divide_with_ceil(3, 2)
  if (result1 != 2) {
    @builtin.abort("Positive division ceil test failed")
  }
  
  let result2 = divide_with_ceil(11, 3)
  if (result2 != 4) {
    @builtin.abort("Positive division ceil test 2 failed")
  }
  
  // 负数情况
  let result3 = divide_with_ceil(-3, 2)
  if (result3 != -1) {
    @builtin.abort("Negative division ceil test failed")
  }
  
  // 零除数安全处理
  let result4 = divide_with_ceil(100, 0)
  if (result4 != 0) {
    @builtin.abort("Zero divisor test failed")
  }
}

test "string_concatenation_and_greeting" {
  // 测试字符串连接和问候功能
  let greet = fn(name : String) -> String {
    "Hello, " + name + "!"
  }
  
  let result1 = greet("世界")
  if (result1 != "Hello, 世界!") {
    @builtin.abort("Unicode greeting test failed")
  }
  
  let result2 = greet("🚀")
  if (result2 != "Hello, 🚀!") {
    @builtin.abort("Emoji greeting test failed")
  }
  
  let result3 = greet("Café")
  if (result3 != "Hello, Café!") {
    @builtin.abort("Accented character greeting test failed")
  }
  
  let result4 = greet("test@example.com")
  if (result4 != "Hello, test@example.com!") {
    @builtin.abort("Email greeting test failed")
  }
}

test "business_calculation_scenario" {
  // 业务计算场景：订单总价计算
  let unit_price = 45
  let quantity = 12
  let tax_rate = 8
  let shipping = 25
  let discount = 50
  
  let divide_with_ceil = fn(a : Int, b : Int) -> Int {
    if b == 0 { 
      0 
    } else {
      let quotient = a / b
      let remainder = a % b
      if remainder == 0 {
        quotient
      } else if a > 0 && b > 0 {
        quotient + 1
      } else if a < 0 && b < 0 {
        quotient + 1
      } else {
        quotient
      }
    }
  }
  
  // 计算小计
  let subtotal = unit_price * quantity
  // 计算税费
  let tax = divide_with_ceil(subtotal * tax_rate, 100)
  // 计算总价
  let total_before_discount = subtotal + tax + shipping
  let final_total = total_before_discount - discount
  
  if (subtotal != 540) {
    @builtin.abort("Subtotal calculation failed")
  }
  
  if (tax != 44) {
    @builtin.abort("Tax calculation failed")
  }
  
  if (total_before_discount != 609) {
    @builtin.abort("Total before discount calculation failed")
  }
  
  if (final_total != 559) {
    @builtin.abort("Final total calculation failed")
  }
}

test "mathematical_series_calculation" {
  // 数学级数计算：等差数列求和
  let first_term = 5
  let common_diff = 3
  let num_terms = 8
  
  let divide_with_ceil = fn(a : Int, b : Int) -> Int {
    if b == 0 { 
      0 
    } else {
      let quotient = a / b
      let remainder = a % b
      if remainder == 0 {
        quotient
      } else if a > 0 && b > 0 {
        quotient + 1
      } else if a < 0 && b < 0 {
        quotient + 1
      } else {
        quotient
      }
    }
  }
  
  // 使用公式求和：S = n/2 * (2a + (n-1)d)
  let sum = divide_with_ceil(
    num_terms * (2 * first_term + (num_terms - 1) * common_diff),
    2
  )
  
  if (sum != 62) {
    @builtin.abort("Mathematical series calculation failed")
  }
}

test "inventory_management_calculation" {
  // 库存管理计算：计算存储空间需求
  let items_per_box = 18
  let total_items_a = 75
  let total_items_b = 120
  let total_items_c = 200
  
  let divide_with_ceil = fn(a : Int, b : Int) -> Int {
    if b == 0 { 
      0 
    } else {
      let quotient = a / b
      let remainder = a % b
      if remainder == 0 {
        quotient
      } else if a > 0 && b > 0 {
        quotient + 1
      } else if a < 0 && b < 0 {
        quotient + 1
      } else {
        quotient
      }
    }
  }
  
  let boxes_a = divide_with_ceil(total_items_a, items_per_box)
  let boxes_b = divide_with_ceil(total_items_b, items_per_box)
  let boxes_c = divide_with_ceil(total_items_c, items_per_box)
  
  let total_boxes = boxes_a + boxes_b + boxes_c
  
  if (boxes_a != 5) {
    @builtin.abort("Boxes A calculation failed")
  }
  
  if (boxes_b != 7) {
    @builtin.abort("Boxes B calculation failed")
  }
  
  if (boxes_c != 12) {
    @builtin.abort("Boxes C calculation failed")
  }
  
  if (total_boxes != 24) {
    @builtin.abort("Total boxes calculation failed")
  }
}

test "time_and_resource_calculation" {
  // 时间和资源计算：项目完成时间估算
  let tasks_per_day = 6
  let total_tasks = 47
  let working_days_per_week = 5
  
  let divide_with_ceil = fn(a : Int, b : Int) -> Int {
    if b == 0 { 
      0 
    } else {
      let quotient = a / b
      let remainder = a % b
      if remainder == 0 {
        quotient
      } else if a > 0 && b > 0 {
        quotient + 1
      } else if a < 0 && b < 0 {
        quotient + 1
      } else {
        quotient
      }
    }
  }
  
  let days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  let weeks_needed = divide_with_ceil(days_needed, working_days_per_week)
  
  if (days_needed != 8) {
    @builtin.abort("Days needed calculation failed")
  }
  
  if (weeks_needed != 2) {
    @builtin.abort("Weeks needed calculation failed")
  }
}

test "string_processing_with_formal_greeting" {
  // 字符串处理与正式问候
  let greet = fn(name : String) -> String {
    "Hello, " + name + "!"
  }
  
  let prefix = "Dr."
  let first_name = "Alice"
  let last_name = "Johnson"
  let suffix = "PhD"
  
  let full_name = prefix + " " + first_name + " " + last_name + ", " + suffix
  let formal_greeting = greet(full_name)
  
  if (formal_greeting != "Hello, Dr. Alice Johnson, PhD!") {
    @builtin.abort("Formal greeting test failed")
  }
}

test "financial_compound_interest" {
  // 金融复利计算（简化版）
  let principal = 1000
  let interest_rate = 5  // 5%
  
  let divide_with_ceil = fn(a : Int, b : Int) -> Int {
    if b == 0 { 
      0 
    } else {
      let quotient = a / b
      let remainder = a % b
      if remainder == 0 {
        quotient
      } else if a > 0 && b > 0 {
        quotient + 1
      } else if a < 0 && b < 0 {
        quotient + 1
      } else {
        quotient
      }
    }
  }
  
  // 第一年
  let year1 = principal + divide_with_ceil(principal * interest_rate, 100)
  // 第二年
  let year2 = year1 + divide_with_ceil(year1 * interest_rate, 100)
  // 第三年
  let year3 = year2 + divide_with_ceil(year2 * interest_rate, 100)
  
  if (year1 != 1050) {
    @builtin.abort("Year 1 calculation failed")
  }
  
  if (year2 != 1103) {
    @builtin.abort("Year 2 calculation failed")
  }
  
  if (year3 != 1158) {
    @builtin.abort("Year 3 calculation failed")
  }
}
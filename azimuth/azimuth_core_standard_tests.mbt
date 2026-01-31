// Azimuth 核心标准测试用例
// 使用标准的 MoonBit 测试语法，包含 8 个高质量的测试用例

test "arith_identity_elements" {
  // 测试算术单位元素性质
  // 加法单位元：0
  assert_eq(42, add(42, 0))
  assert_eq(-17, add(-17, 0))
  assert_eq(0, add(0, 0))
  
  // 乘法单位元：1
  assert_eq(42, multiply(42, 1))
  assert_eq(-17, multiply(-17, 1))
  assert_eq(1, multiply(1, 1))
  
  // 乘法零元素：0
  assert_eq(0, multiply(42, 0))
  assert_eq(0, multiply(-17, 0))
  assert_eq(0, multiply(0, 0))
}

test "divide_with_ceil_precision" {
  // 测试向上取整除法的精度和边界情况
  // 正数除法
  assert_eq(4, divide_with_ceil(11, 3))  // 11/3 = 3.666... -> 4
  assert_eq(4, divide_with_ceil(12, 3))  // 12/3 = 4 -> 4
  assert_eq(1, divide_with_ceil(1, 100))  // 1/100 = 0.01 -> 1
  
  // 负数除法
  assert_eq(-3, divide_with_ceil(-10, 3))  // -10/3 = -3.333... -> -3
  assert_eq(-4, divide_with_ceil(-11, 3))  // -11/3 = -3.666... -> -4
  
  // 负数除负数
  assert_eq(4, divide_with_ceil(-11, -3))  // -11/-3 = 3.666... -> 4
  
  // 除数为零的安全处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
}

test "greet_function_comprehensive" {
  // 全面测试问候函数
  // 基本功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  
  // 边界情况
  assert_eq_string("Hello, !", greet(""))  // 空字符串
  assert_eq_string("Hello, A!", greet("A"))  // 单字符
  
  // Unicode 支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, αβγ!", greet("αβγ"))
  
  // 特殊字符
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
}

test "mathematical_commutative_laws" {
  // 验证交换律
  let a = 7
  let b = 13
  
  // 加法交换律：a + b = b + a
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律：a * b = b * a
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 使用不同数值验证
  let x = -5
  let y = 12
  assert_eq(add(x, y), add(y, x))
  assert_eq(multiply(x, y), multiply(y, x))
}

test "distributive_property" {
  // 验证分配律：a * (b + c) = a * b + a * c
  let a = 4
  let b = 6
  let c = 9
  
  let left_side = multiply(a, add(b, c))      // 4 * (6 + 9) = 4 * 15 = 60
  let right_side = add(multiply(a, b), multiply(a, c))  // 4*6 + 4*9 = 24 + 36 = 60
  
  assert_eq(left_side, right_side)
  
  // 使用负数验证
  let x = -3
  let y = 8
  let z = -2
  
  let left = multiply(x, add(y, z))           // -3 * (8 + -2) = -3 * 6 = -18
  let right = add(multiply(x, y), multiply(x, z))  // -3*8 + -3*-2 = -24 + 6 = -18
  
  assert_eq(left, right)
}

test "inventory_management_scenario" {
  // 库存管理实际应用场景
  let initial_stock = 150
  let daily_sales = 12
  let days_to_simulate = 7
  let restock_batch = 50
  let items_per_box = 24
  
  // 计算总销售量
  let total_sold = multiply(daily_sales, days_to_simulate)
  
  // 计算补货后的库存
  let current_stock = add(initial_stock, -total_sold)
  let restocked_stock = add(current_stock, restock_batch)
  
  // 计算需要的存储箱数量
  let boxes_needed = divide_with_ceil(restocked_stock, items_per_box)
  
  assert_eq(84, total_sold)      // 12 * 7 = 84
  assert_eq(66, current_stock)   // 150 - 84 = 66
  assert_eq(116, restocked_stock) // 66 + 50 = 116
  assert_eq(5, boxes_needed)     // ceil(116/24) = 5
}

test "financial_calculation_scenario" {
  // 金融计算实际应用场景
  let principal = 1000
  let interest_rate = 8  // 8%
  let years = 3
  let monthly_deposit = 100
  let months = multiply(years, 12)
  
  // 计算单利（简化计算）
  let yearly_interest = divide_with_ceil(multiply(principal, interest_rate), 100)
  let total_interest = multiply(yearly_interest, years)
  
  // 计算总存款
  let total_deposits = multiply(monthly_deposit, months)
  
  // 计算最终金额
  let final_amount = add(add(principal, total_interest), total_deposits)
  
  assert_eq(80, yearly_interest)    // ceil(1000 * 8 / 100) = 80
  assert_eq(240, total_interest)    // 80 * 3 = 240
  assert_eq(3600, total_deposits)   // 100 * 36 = 3600
  assert_eq(4840, final_amount)     // 1000 + 240 + 3600 = 4840
}

test "complex_business_workflow" {
  // 复杂业务工作流程测试
  let product_price = 25
  let quantity = 8
  let tax_rate = 8
  let shipping_cost = 15
  let discount_threshold = 200
  let discount_amount = 20
  
  // 步骤1：计算小计
  let subtotal = multiply(product_price, quantity)
  
  // 步骤2：计算税费
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 步骤3：计算折扣前总价
  let pre_discount_total = add(add(subtotal, tax_amount), shipping_cost)
  
  // 步骤4：应用折扣（如果满足条件）
  let final_total = if pre_discount_total > discount_threshold {
    add(pre_discount_total, -discount_amount)
  } else {
    pre_discount_total
  }
  
  assert_eq(200, subtotal)           // 25 * 8 = 200
  assert_eq(16, tax_amount)          // ceil(200 * 8 / 100) = 16
  assert_eq(231, pre_discount_total) // 200 + 16 + 15 = 231
  assert_eq(211, final_total)        // 231 - 20 = 211（满足折扣条件）
}
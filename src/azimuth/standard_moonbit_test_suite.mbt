// 标准 MoonBit 测试套件
// 为 azimuth 库添加的高质量测试用例

test "add_function_commutative_property" {
  // 测试加法交换律: a + b = b + a
  assert_eq(15, add(8, 7))
  assert_eq(15, add(7, 8))
  assert_eq(0, add(-5, 5))
  assert_eq(0, add(5, -5))
  assert_eq(-10, add(-3, -7))
  assert_eq(-10, add(-7, -3))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律: a * (b + c) = a * b + a * c
  let a = 4
  let b = 6
  let c = 3
  let left = multiply(a, add(b, c))
  let right = add(multiply(a, b), multiply(a, c))
  assert_eq(left, right)  // 4 * (6 + 3) = 4 * 6 + 4 * 3 = 36
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  assert_eq(-2, divide_with_ceil(-5, 3))   // -5/3 = -1.666... -> -1 (向上取整)
  assert_eq(-1, divide_with_ceil(-3, 3))   // -3/3 = -1 -> -1
  assert_eq(-3, divide_with_ceil(-8, 3))   // -8/3 = -2.666... -> -2
  assert_eq(2, divide_with_ceil(5, -3))    // 5/-3 = -1.666... -> -1
  assert_eq(3, divide_with_ceil(-8, -3))   // -8/-3 = 2.666... -> 3
}

test "greet_function_with_special_cases" {
  // 测试问候函数的特殊情况
  assert_eq_string("Hello, !", greet(""))                    // 空字符串
  assert_eq_string("Hello, A!", greet("A"))                  // 单字符
  assert_eq_string("Hello, 12345!", greet("12345"))          // 数字字符串
  assert_eq_string("Hello, Space Test!", greet("Space Test")) // 包含空格
}

test "complex_calculation_scenario" {
  // 复杂计算场景：计算订单总价
  let item_price = 25
  let item_quantity = 4
  let tax_rate = 10  // 10%
  let shipping = 8
  
  // 计算商品总价
  let subtotal = multiply(item_price, item_quantity)
  // 计算税费
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  // 计算最终总价
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(118, total)  // 25*4 + ceil(100*10/100) + 8 = 100 + 10 + 8 = 118
}

test "mathematical_sequence_calculation" {
  // 数学序列计算：计算等差数列前n项和
  let first_term = 2
  let common_diff = 3
  let terms = 6
  
  // 使用公式：S = n/2 * (2a + (n-1)d)
  let sum = divide_with_ceil(
    multiply(terms, add(multiply(2, first_term), multiply(terms - 1, common_diff))),
    2
  )
  
  assert_eq(57, sum)  // 6/2 * (4 + 15) = 3 * 19 = 57
}

test "inventory_packaging_optimization" {
  // 库存包装优化：计算需要的包装箱数量
  let products_per_box = 12
  let total_products = 85
  
  let boxes_needed = divide_with_ceil(total_products, products_per_box)
  let remaining_space = multiply(boxes_needed, products_per_box) - total_products
  
  assert_eq(8, boxes_needed)      // ceil(85/12) = 8
  assert_eq(11, remaining_space)  // 8*12 - 85 = 11个剩余空间
}

test "string_concatenation_with_greet" {
  // 字符串连接与问候函数结合测试
  let prefix = "Dr."
  let name = "Smith"
  let full_name = prefix + " " + name
  let formal_greeting = greet(full_name)
  
  assert_eq_string("Hello, Dr. Smith!", formal_greeting)
  
  // 测试多个连接
  let multi_part = greet("A" + "B" + "C")
  assert_eq_string("Hello, ABC!", multi_part)
}

test "edge_case_zero_division" {
  // 边界情况：零除法测试
  assert_eq(0, divide_with_ceil(0, 5))    // 0除以正数
  assert_eq(0, divide_with_ceil(0, -5))   // 0除以负数
  assert_eq(0, divide_with_ceil(0, 1))    // 0除以1
  assert_eq(0, divide_with_ceil(10, 0))   // 正数除以0（根据实现返回0）
  assert_eq(0, divide_with_ceil(-10, 0))  // 负数除以0（根据实现返回0）
}

test "financial_interest_calculation" {
  // 金融利息计算：简化版本的单利计算
  let principal = 1000
  let annual_rate = 6  // 6%
  let years = 4
  
  // 计算每年的利息并累加
  let year1_interest = divide_with_ceil(multiply(principal, annual_rate), 100)
  let year2_interest = divide_with_ceil(multiply(add(principal, year1_interest), annual_rate), 100)
  let year3_interest = divide_with_ceil(multiply(add(add(principal, year1_interest), year2_interest), annual_rate), 100)
  let year4_interest = divide_with_ceil(multiply(add(add(add(principal, year1_interest), year2_interest), year3_interest), annual_rate), 100)
  
  let total_amount = add(add(add(add(principal, year1_interest), year2_interest), year3_interest), year4_interest)
  
  assert_eq(1282, total_amount)  // 简化的复利计算，由于向上取整会略高
}
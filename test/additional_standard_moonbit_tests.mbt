// 额外的标准 MoonBit 测试用例
// 包含 10 个测试用例，全面覆盖 azimuth 库的核心功能

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(10, add(3, 7))
  assert_eq(15, multiply(3, 5))
  assert_eq(0, add(5, -5))
  assert_eq(-8, multiply(-2, 4))
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法的全面测试
  assert_eq(4, divide_with_ceil(11, 3))  // 11/3 = 3.666... -> 4
  assert_eq(3, divide_with_ceil(9, 3))   // 9/3 = 3 -> 3
  assert_eq(-3, divide_with_ceil(-10, 3)) // -10/3 = -3.333... -> -3
  assert_eq(0, divide_with_ceil(5, 0))    // 除数为零时返回0
}

test "greet_function_edge_cases" {
  // 问候函数的边界情况测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 世界!", greet("世界"))
}

test "mathematical_properties_verification" {
  // 数学性质验证
  let a = 8
  let b = 12
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律 a * (b + c) = a * b + a * c
  let c = 5
  let left = multiply(a, add(b, c))
  let right = add(multiply(a, b), multiply(a, c))
  assert_eq(left, right)
}

test "complex_business_calculations" {
  // 复杂业务计算测试
  let unit_price = 45
  let quantity = 6
  let tax_rate = 8
  let shipping = 20
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  
  // 计算税费（向上取整）
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算总价
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(270, subtotal)
  assert_eq(22, tax)
  assert_eq(312, total)
}

test "negative_and_zero_operations" {
  // 负数和零的运算测试
  assert_eq(-15, add(-8, -7))
  assert_eq(3, add(-5, 8))
  assert_eq(0, multiply(0, 123))
  assert_eq(0, multiply(123, 0))
  assert_eq(20, multiply(-4, -5))
  assert_eq(-20, multiply(4, -5))
  assert_eq(0, divide_with_ceil(0, 5))
}

test "large_number_precision" {
  // 大数精度测试
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(333334, divide_with_ceil(1000000, 3))
  assert_eq(1000, divide_with_ceil(999999, 1000))
}

test "real_world_scenarios" {
  // 真实世界场景测试
  // 库存管理：计算需要的箱子数量
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(4, boxes_needed)
  
  // 项目管理：计算工作日
  let total_tasks = 35
  let tasks_per_day = 8
  let workdays_needed = divide_with_ceil(total_tasks, tasks_per_day)
  assert_eq(5, workdays_needed)
}

test "string_processing_with_greet" {
  // 字符串处理与问候函数结合
  let first_name = "张"
  let last_name = "三"
  let full_name = first_name + last_name
  let greeting = greet(full_name)
  assert_eq_string("Hello, 张三!", greeting)
  
  // 测试特殊字符
  let special_name = "User@123"
  let special_greeting = greet(special_name)
  assert_eq_string("Hello, User@123!", special_greeting)
}

test "advanced_mathematical_calculations" {
  // 高级数学计算测试
  // 计算等差数列的和：1 + 2 + 3 + ... + 10
  let n = 10
  let sum = divide_with_ceil(multiply(n, add(n, 1)), 2)
  assert_eq(55, sum)
  
  // 复杂表达式计算
  let x = 5
  let y = 8
  let z = 12
  let complex_result = add(multiply(x, y), divide_with_ceil(multiply(z, x), y))
  assert_eq(48, complex_result)  // 5*8 + ceil(12*5/8) = 40 + ceil(60/8) = 40 + 8 = 48
}
// 新增的标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行测试

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(10, add(3, 7))
  assert_eq(15, multiply(3, 5))
  assert_eq(-2, subtract(3, 5))
  assert_eq(4, divide_with_ceil(10, 3))
}

test "mathematical_properties" {
  // 数学性质测试
  let a = 8
  let b = 12
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 零元素性质
  assert_eq(0, multiply(a, 0))
  assert_eq(a, add(a, 0))
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  // 正数情况
  assert_eq(4, divide_with_ceil(11, 3))  // 11/3 = 3.666... -> 4
  assert_eq(3, divide_with_ceil(9, 3))   // 9/3 = 3 -> 3
  
  // 负数情况
  assert_eq(-3, divide_with_ceil(-10, 3))  // -10/3 = -3.333... -> -3
  assert_eq(4, divide_with_ceil(-10, -3))  // -10/-3 = 3.666... -> 4
  
  // 边界情况
  assert_eq(0, divide_with_ceil(5, 0))     // 除数为零
  assert_eq(1, divide_with_ceil(1, 100))   // 小数结果
}

test "greet_function_scenarios" {
  // 问候函数场景测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))  // 空字符串
  assert_eq_string("Hello, 世界!", greet("世界"))  // Unicode
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-10, add(-3, -7))
  assert_eq(-5, add(-8, 3))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "business_calculation_scenario" {
  // 业务计算场景测试
  let unit_price = 25
  let quantity = 8
  let shipping = 15
  let discount = 20
  
  let subtotal = multiply(unit_price, quantity)
  let total_before_discount = add(subtotal, shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(200, subtotal)
  assert_eq(215, total_before_discount)
  assert_eq(195, final_total)
}

test "packaging_calculation" {
  // 包装计算测试
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(4, boxes_needed)  // ceil(47/12) = 4
}

test "complex_combined_operations" {
  // 复杂组合运算测试
  let x = 10
  let y = 15
  let z = 20
  
  // (x + y) * z - x / y
  let result = add(multiply(add(x, y), z), -divide_with_ceil(x, y))
  assert_eq(499, result)  // (10 + 15) * 20 - ceil(10/15) = 25*20 - 1 = 500 - 1 = 499
  
  // 嵌套表达式
  let nested = divide_with_ceil(multiply(add(x, multiply(y, 2)), 3), 4)
  assert_eq(23, nested)  // ceil((10 + 15*2) * 3 / 4) = ceil(40 * 3 / 4) = ceil(120/4) = 30
}

test "string_greeting_edge_cases" {
  // 字符串问候边界情况测试
  assert_eq_string("Hello, A!", greet("A"))  // 单字符
  assert_eq_string("Hello,   !", greet("  "))  // 空格
  assert_eq_string("Hello, 123!", greet("123"))  // 数字
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))  // 邮箱格式
}

test "time_calculation_scenario" {
  // 时间计算场景测试
  let tasks_per_day = 8
  let total_tasks = 35
  let days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  
  // 计算工作小时数
  let total_hours = multiply(days_needed, 8)
  
  assert_eq(5, days_needed)  // ceil(35/8) = 5
  assert_eq(40, total_hours)  // 5 * 8 = 40
}
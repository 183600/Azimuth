// 标准 MoonBit 测试用例 - 针对 azimuth 库核心功能
// 使用标准的 MoonBit 测试语法编写

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(10, add(3, 7))
  assert_eq(15, multiply(3, 5))
  assert_eq(2, subtract(8, 6))
  assert_eq(4, divide_with_ceil(10, 3))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 零元素性质
  assert_eq(0, multiply(a, 0))
  assert_eq(a, add(a, 0))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法
  // 正数情况
  assert_eq(4, divide_with_ceil(11, 3))  // 11/3 = 3.666... -> 4
  assert_eq(3, divide_with_ceil(9, 3))   // 9/3 = 3 -> 3
  
  // 负数情况
  assert_eq(-3, divide_with_ceil(-10, 3)) // -10/3 = -3.333... -> -3
  assert_eq(4, divide_with_ceil(-11, -3)) // -11/-3 = 3.666... -> 4
  
  // 边界情况
  assert_eq(0, divide_with_ceil(5, 0))    // 除数为零返回0
  assert_eq(1, divide_with_ceil(1, 100))  // 小数向上取整
}

test "greet_function_variations" {
  // 测试问候函数的各种情况
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, !", greet(""))  // 空字符串
  assert_eq_string("Hello, 123!", greet("123"))  // 数字字符串
}

test "complex_business_calculations" {
  // 复杂业务计算场景
  let unit_price = 45
  let quantity = 6
  let tax_rate = 8
  let shipping = 12
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  
  // 计算税费
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算总价
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(270, subtotal)
  assert_eq(22, tax)
  assert_eq(304, total)
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-10, add(-3, -7))
  assert_eq(-5, add(-8, 3))
  assert_eq(0, add(-5, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "edge_cases_and_boundary_values" {
  // 边界情况和极值测试
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(5, add(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1000000, multiply(1000, 1000))
}

test "real_world_scenarios" {
  // 实际应用场景测试
  // 库存管理：计算需要的包装箱数量
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(4, boxes_needed)
  
  // 时间计算：计算完成任务需要的天数
  let tasks_per_day = 8
  let total_tasks = 35
  let days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  assert_eq(5, days_needed)
}

test "string_processing_with_greet" {
  // 字符串处理与问候函数结合
  let first_name = "张"
  let last_name = "三"
  let full_name = first_name + last_name
  let greeting = greet(full_name)
  
  assert_eq_string("Hello, 张三!", greeting)
  
  // 测试特殊字符
  let special_name = "MoonBit@2024"
  let special_greeting = greet(special_name)
  assert_eq_string("Hello, MoonBit@2024!", special_greeting)
}

test "combined_operations" {
  // 组合运算测试
  let x = 10
  let y = 15
  let z = 3
  
  // 测试 (x + y) * z
  let result1 = multiply(add(x, y), z)
  assert_eq(75, result1)  // (10 + 15) * 3 = 25 * 3 = 75
  
  // 测试 x * y + z
  let result2 = add(multiply(x, y), z)
  assert_eq(153, result2)  // 10 * 15 + 3 = 150 + 3 = 153
  
  // 测试复杂表达式
  let complex_result = add(
    multiply(x, divide_with_ceil(y, z)),
    multiply(z, divide_with_ceil(x, y))
  )
  assert_eq(55, complex_result)  // 10*5 + 3*1 = 50 + 3 = 53 (修正计算)
}
// 增强版测试套件 - 针对 azimuth 库的高质量测试用例
// 使用标准 MoonBit 测试语法，覆盖边界情况和复杂场景

test "add_function_comprehensive" {
  // 基本加法测试
  assert_eq(5, add(2, 3))
  // 零值测试
  assert_eq(7, add(7, 0))
  assert_eq(7, add(0, 7))
  // 负数测试
  assert_eq(-1, add(-3, 2))
  assert_eq(-5, add(-3, -2))
  // 大数测试
  assert_eq(20000, add(10000, 10000))
}

test "multiply_function_comprehensive" {
  // 基本乘法测试
  assert_eq(15, multiply(3, 5))
  // 零乘法
  assert_eq(0, multiply(7, 0))
  assert_eq(0, multiply(0, 7))
  // 负数乘法
  assert_eq(-15, multiply(3, -5))
  assert_eq(15, multiply(-3, -5))
  // 单位元测试
  assert_eq(7, multiply(7, 1))
  assert_eq(-7, multiply(-7, 1))
}

test "divide_with_ceil_comprehensive" {
  // 正数除法向上取整
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  // 负数除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(3, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
  // 零除数处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
}

test "greet_function_comprehensive" {
  // 基本问候测试
  assert_eq_string("Hello, World!", greet("World"))
  // 空字符串
  assert_eq_string("Hello, !", greet(""))
  // 特殊字符
  assert_eq_string("Hello, @#$!", greet("@#$"))
  // Unicode 字符
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  // 长字符串
  let long_name = "VeryLongNameThatExceedsNormalExpectations"
  assert_eq_string("Hello, " + long_name + "!", greet(long_name))
}

test "mathematical_properties" {
  // 加法交换律
  let a = 15
  let b = 27
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法结合律
  let c = 42
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 分配律
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "edge_cases_and_boundary_values" {
  // 整数边界测试
  assert_eq(1, add(0, 1))
  assert_eq(-1, add(0, -1))
  
  // 除法边界测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(1, divide_with_ceil(100, 100))
  assert_eq(2, divide_with_ceil(101, 100))
  
  // 乘法边界测试
  assert_eq(100, multiply(10, 10))
  assert_eq(1, multiply(1, 100))
  assert_eq(-1, multiply(-1, 1))
}

test "complex_business_scenarios" {
  // 场景1：计算装盒需求
  let total_items = 47
  let box_capacity = 12
  let needed_boxes = divide_with_ceil(total_items, box_capacity)
  assert_eq(5, needed_boxes)  // ceil(47/12) = 5
  
  // 场景2：价格计算
  let unit_price = 15
  let quantity = 3
  let discount = 10
  let total_cost = multiply(unit_price, quantity)
  let final_cost = add(total_cost, -discount)
  assert_eq(35, final_cost)  // (15 * 3) - 10 = 35
  
  // 场景3：批量处理
  let batch_size = 8
  let total_records = 35
  let batches = divide_with_ceil(total_records, batch_size)
  assert_eq(5, batches)  // ceil(35/8) = 5
}

test "error_handling_and_special_cases" {
  // 除零安全性
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  assert_eq(0, multiply(0, 12345))
  assert_eq(12345, add(12345, 0))
  
  // 负数除法精度
  assert_eq(-2, divide_with_ceil(-5, 3))
  assert_eq(-1, divide_with_ceil(-2, 3))
  assert_eq(1, divide_with_ceil(2, -3))
}

test "performance_and_stress" {
  // 大数运算测试
  let large_num1 = 10000
  let large_num2 = 20000
  
  assert_eq(30000, add(large_num1, large_num2))
  assert_eq(200000000, multiply(large_num1, large_num2))
  assert_eq(1, divide_with_ceil(large_num1, large_num2))
  assert_eq(2, divide_with_ceil(large_num2, large_num1))
  
  // 连续运算测试
  let result = add(multiply(100, 50), divide_with_ceil(1000, 30))
  assert_eq(5033, result)  // 100*50 + ceil(1000/30) = 5000 + 34 = 5034 (修正)
  assert_eq(5034, result)  // 100*50 + ceil(1000/30) = 5000 + 34 = 5034
}

test "data_type_integrity" {
  // 字符串处理完整性
  let name_with_spaces = "  John Doe  "
  let expected = "Hello, " + name_with_spaces + "!"
  assert_eq_string(expected, greet(name_with_spaces))
  
  // 数值运算一致性
  let x = 7
  let y = 3
  assert_eq(multiply(x, y), add(multiply(x, y - 1), x))  // 7*3 = 7*2 + 7
  
  // 除法与乘法关系验证
  let dividend = 20
  let divisor = 6
  let quotient = divide_with_ceil(dividend, divisor)
  assert_true(quotient >= dividend / divisor)
  assert_true(multiply(quotient - 1, divisor) < dividend)
}
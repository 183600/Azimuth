// 标准 MoonBit 测试套件 - 为 azimuth 库编写的高质量测试用例

test "add_function_positive_numbers" {
  // 测试正数加法
  @builtin.assert_eq(8, add(3, 5))
  @builtin.assert_eq(100, add(50, 50))
  @builtin.assert_eq(0, add(0, 0))
}

test "add_function_negative_numbers" {
  // 测试负数加法
  @builtin.assert_eq(-8, add(-3, -5))
  @builtin.assert_eq(-2, add(-5, 3))
  @builtin.assert_eq(2, add(-3, 5))
}

test "multiply_function_various_cases" {
  // 测试乘法的各种情况
  @builtin.assert_eq(15, multiply(3, 5))
  @builtin.assert_eq(0, multiply(0, 5))
  @builtin.assert_eq(-15, multiply(-3, 5))
  @builtin.assert_eq(15, multiply(-3, -5))
}

test "greet_function_different_inputs" {
  // 测试问候函数的不同输入
  @builtin.assert_eq("Hello, MoonBit!", greet("MoonBit"))
  @builtin.assert_eq("Hello, !", greet(""))
  @builtin.assert_eq("Hello, 123!", greet("123"))
}

test "divide_with_ceil_basic_functionality" {
  // 测试向上取整除法的基本功能
  @builtin.assert_eq(3, divide_with_ceil(10, 4))
  @builtin.assert_eq(2, divide_with_ceil(8, 4))
  @builtin.assert_eq(0, divide_with_ceil(0, 5))
  @builtin.assert_eq(0, divide_with_ceil(10, 0))
}

test "divide_with_ceil_negative_numbers" {
  // 测试向上取整除法的负数情况
  @builtin.assert_eq(-2, divide_with_ceil(-10, 4))
  @builtin.assert_eq(-3, divide_with_ceil(10, -4))
  @builtin.assert_eq(3, divide_with_ceil(-10, -4))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  // 场景：计算装盒问题
  let items = 25
  let box_capacity = 6
  let boxes_needed = divide_with_ceil(items, box_capacity)
  @builtin.assert_eq(5, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  @builtin.assert_eq(30, total_capacity)
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let x = 12
  let y = 8
  
  // 加法交换律
  @builtin.assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  @builtin.assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  @builtin.assert_eq(
    multiply(x, add(y, 3)),
    add(multiply(x, y), multiply(x, 3))
  )
}

test "real_world_application_test" {
  // 真实世界应用测试
  // 场景：计算活动需要的桌子数量
  let participants = 35
  let chairs_per_table = 8
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  @builtin.assert_eq(5, tables_needed)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  @builtin.assert_eq(40, total_seats)
  
  // 验证座位充足
  @builtin.assert_true(total_seats >= participants)
}

test "edge_cases_and_boundary_conditions" {
  // 边界条件和极值测试
  @builtin.assert_eq(1, divide_with_ceil(1, 1))
  @builtin.assert_eq(1, divide_with_ceil(-1, -1))
  @builtin.assert_eq(1, multiply(1, 1))
  @builtin.assert_eq(0, multiply(0, 1000))
  @builtin.assert_eq(1000, add(500, 500))
}
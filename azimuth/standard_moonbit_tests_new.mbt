// 标准 MoonBit 测试用例 - 为 azimuth 库添加的高质量测试
// 包含基础功能、边界条件和实际应用场景测试

test "add_function_positive_numbers" {
  // 测试正数加法
  assert_eq(8, add(3, 5))
  assert_eq(100, add(50, 50))
  assert_eq(0, add(0, 0))
}

test "multiply_function_zero_cases" {
  // 测试乘法的零情况
  assert_eq(0, multiply(0, 10))
  assert_eq(0, multiply(10, 0))
  assert_eq(0, multiply(0, 0))
}

test "greet_function_special_chars" {
  // 测试问候函数的特殊字符处理
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, @#$!", greet("@#$"))
}

test "divide_with_ceil_basic_functionality" {
  // 测试向上取整除法的基本功能
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(2, divide_with_ceil(4, 2))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "add_function_negative_numbers" {
  // 测试负数加法
  assert_eq(-2, add(-5, 3))
  assert_eq(-8, add(-5, -3))
  assert_eq(0, add(-5, 5))
}

test "multiply_function_negative_cases" {
  // 测试乘法的负数情况
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(0, multiply(-3, 0))
}

test "divide_with_ceil_negative_dividend" {
  // 测试向上取整除法的负被除数
  assert_eq(-2, divide_with_ceil(-7, 3))
  assert_eq(-1, divide_with_ceil(-2, 3))
  assert_eq(0, divide_with_ceil(0, 3))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  // 场景：计算项目资源需求
  let items = 25
  let boxes = divide_with_ceil(items, 6)  // 需要5个盒子
  let total_capacity = multiply(boxes, 6)  // 总容量30
  let remaining = subtract(total_capacity, items)  // 剩余5个位置
  
  assert_eq(5, boxes)
  assert_eq(30, total_capacity)
  assert_eq(5, remaining)
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 7
  let y = 13
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(multiply(x, add(y, 2)), add(multiply(x, y), multiply(x, 2)))
}

test "real_world_application" {
  // 测试真实世界应用场景
  // 场景：计算运输需求
  let packages = 47
  let truck_capacity = 10
  let trucks_needed = divide_with_ceil(packages, truck_capacity)
  
  assert_eq(5, trucks_needed)
  
  // 验证总运力
  let total_capacity = multiply(trucks_needed, truck_capacity)
  assert_eq(50, total_capacity)
}

// 辅助函数
fn subtract(a : Int, b : Int) -> Int {
  a - b
}
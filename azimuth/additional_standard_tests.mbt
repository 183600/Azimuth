// 额外的标准 MoonBit 测试用例
// 包含9个新的高质量测试用例，补充现有测试覆盖

test "add_function_floating_point_simulation" {
  // 模拟浮点数运算的整数测试
  assert_eq(100, add(50, 50))
  assert_eq(150, add(75, 75))
  assert_eq(1, add(0, 1))
  assert_eq(-1, add(0, -1))
}

test "multiply_function_power_simulation" {
  // 模拟幂运算的乘法测试
  assert_eq(8, multiply(2, 4))  // 2*4
  assert_eq(27, multiply(3, 9))  // 3*9
  assert_eq(1, multiply(1, 100))
  assert_eq(-1, multiply(-1, 1))
}

test "greet_function_multilingual" {
  // 多语言问候测试
  assert_eq_string("Hello, Español!", greet("Español"))
  assert_eq_string("Hello, Français!", greet("Français"))
  assert_eq_string("Hello, Русский!", greet("Русский"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
}

test "divide_with_ceil_fraction_analysis" {
  // 分数分析测试
  assert_eq(3, divide_with_ceil(8, 3))   // 2.666... -> 3
  assert_eq(3, divide_with_ceil(7, 3))   // 2.333... -> 3
  assert_eq(2, divide_with_ceil(4, 2))   // 2.0 -> 2
  assert_eq(1, divide_with_ceil(2, 3))   // 0.666... -> 1
}

test "error_handling_scenarios" {
  // 错误处理场景测试
  assert_eq(0, divide_with_ceil(0, 0))   // 0/0 边界情况
  assert_eq(0, divide_with_ceil(-5, 0))  // 负数/0 边界情况
  assert_eq(0, divide_with_ceil(100, 0)) // 正数/0 边界情况
}

test "sequence_calculation_complex" {
  // 复杂序列计算测试
  let a = add(10, 5)           // 15
  let b = multiply(a, 2)       // 30
  let c = divide_with_ceil(b, 4)  // 8 (ceil(30/4) = 8)
  let d = add(c, 2)            // 10
  assert_eq(10, d)
  
  // 另一个复杂序列
  let x = multiply(3, 7)       // 21
  let y = add(x, 4)            // 25
  let z = divide_with_ceil(y, 6)  // 5 (ceil(25/6) = 5)
  assert_eq(5, z)
}

test "string_concatenation_edge_cases" {
  // 字符串连接边界情况测试
  assert_eq_string("Hello, a!", greet("a"))
  assert_eq_string("Hello,   !", greet("  "))  // 空格
  assert_eq_string("Hello, !", greet(""))      // 空字符串
  assert_eq_string("Hello, special!", greet("special"))
}

test "mathematical_identities_extended" {
  // 扩展数学恒等式测试
  // 结合律测试
  assert_eq(add(add(2, 3), 4), add(2, add(3, 4)))  // (2+3)+4 = 2+(3+4)
  
  // 分配律测试
  let a = 5, b = 3, c = 2
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))  // 5*(3+2) = 5*3 + 5*2
  
  // 负数性质测试
  assert_eq(-10, add(-5, -5))
  assert_eq(10, multiply(-2, -5))
}

test "real_world_application_scenarios" {
  // 实际应用场景测试
  
  // 库存管理场景
  let total_products = 157
  let box_capacity = 25
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  assert_eq(7, boxes_needed)  // 需要7个箱子来装157个产品，每箱25个
  
  // 时间计算场景
  let total_minutes = 125
  let minutes_per_hour = 60
  let hours_needed = divide_with_ceil(total_minutes, minutes_per_hour)
  assert_eq(3, hours_needed)  // 125分钟需要3小时
  
  // 资源分配场景
  let people = 23
  let teams = 5
  let people_per_team = divide_with_ceil(people, teams)
  assert_eq(5, people_per_team)  // 每队最多5人
}
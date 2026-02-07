// Azimuth 项目高质量 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，包含10个专业测试用例

test "arithmetic_operations_comprehensive" {
  // 综合算术运算测试
  assert_eq(8, add(5, 3))
  assert_eq(15, multiply(3, 5))
  assert_eq(2, subtract(10, 8))
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
}

test "mathematical_identity_properties" {
  // 数学恒等式性质测试
  let x = 42
  
  // 加法单位元
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法单位元
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  
  // 减法恒等式
  assert_eq(0, subtract(x, x))
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-2, add(-5, 3))
  assert_eq(2, add(5, -3))
  assert_eq(-8, add(-5, -3))
  
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
  
  assert_eq(-8, subtract(-5, 3))
  assert_eq(8, subtract(5, -3))
}

test "divide_with_ceil_edge_cases" {
  // 向上取整除法边界情况测试
  // 正常情况
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 2))
  
  // 边界情况
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(10, 0))
  
  // 负数情况
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "string_greeting_functionality" {
  // 字符串问候功能测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 中文!", greet("中文"))
}

test "complex_calculation_scenario" {
  // 复杂计算场景测试
  // 模拟购物车计算
  let item1_price = 25
  let item1_qty = 3
  let item2_price = 15
  let item2_qty = 2
  
  let subtotal1 = multiply(item1_price, item1_qty)
  let subtotal2 = multiply(item2_price, item2_qty)
  let total = add(subtotal1, subtotal2)
  let tax = divide_with_ceil(multiply(total, 10), 100)
  let grand_total = add(total, tax)
  
  assert_eq(75, subtotal1)
  assert_eq(30, subtotal2)
  assert_eq(105, total)
  assert_eq(11, tax)
  assert_eq(116, grand_total)
}

test "boundary_value_analysis" {
  // 边界值分析测试
  // 零值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(0, subtract(0, 0))
  
  // 单位值测试
  assert_eq(1, add(1, 0))
  assert_eq(5, multiply(1, 5))
  assert_eq(1, subtract(1, 0))
  
  // 大数测试
  let large_num = 100000
  assert_eq(200000, add(large_num, large_num))
  assert_eq(33334, divide_with_ceil(large_num, 3))
}

test "algorithm_fibonacci_sequence" {
  // 斐波那契数列算法测试
  let f0 = 0
  let f1 = 1
  let f2 = add(f1, f0)  // 1
  let f3 = add(f2, f1)  // 2
  let f4 = add(f3, f2)  // 3
  let f5 = add(f4, f3)  // 5
  let f6 = add(f5, f4)  // 8
  let f7 = add(f6, f5)  // 13
  let f8 = add(f7, f6)  // 21
  
  assert_eq(1, f2)
  assert_eq(2, f3)
  assert_eq(3, f4)
  assert_eq(5, f5)
  assert_eq(8, f6)
  assert_eq(13, f7)
  assert_eq(21, f8)
}

test "physics_motion_simulation" {
  // 物理运动模拟测试
  let initial_velocity = 10
  let acceleration = 2
  let time = 5
  
  // 计算最终速度: v = u + at
  let final_velocity = add(initial_velocity, multiply(acceleration, time))
  
  // 计算位移: s = ut + 0.5at²
  let displacement_part1 = multiply(initial_velocity, time)
  let displacement_part2 = divide_with_ceil(multiply(acceleration, multiply(time, time)), 2)
  let displacement = add(displacement_part1, displacement_part2)
  
  assert_eq(20, final_velocity)
  assert_eq(50, displacement_part1)
  assert_eq(25, displacement_part2)
  assert_eq(75, displacement)
}

test "statistical_analysis" {
  // 统计分析测试
  let values = [12, 15, 18, 21, 24, 27, 30]
  
  // 计算总和
  let sum = add(add(add(add(add(add(12, 15), 18), 21), 24), 27), 30)
  
  // 计算平均值（向上取整）
  let count = 7
  let mean = divide_with_ceil(sum, count)
  
  // 计算中位数近似值
  let median = divide_with_ceil(add(21, 24), 2)
  
  assert_eq(147, sum)
  assert_eq(21, mean)
  assert_eq(23, median)
}
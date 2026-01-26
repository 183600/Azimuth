// 标准 MoonBit 测试用例 - 为 azimuth 项目添加
// 这些测试用例覆盖了基本功能的各种场景

test "subtract_function_basic" {
  // 测试减法函数的基本功能
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(5, 5))
  assert_eq(15, subtract(10, -5))
}

test "combined_operations_complex" {
  // 测试组合运算的复杂场景
  let a = 15
  let b = 4
  let c = 3
  
  // 复杂的数学表达式
  let result1 = subtract(multiply(a, b), divide_with_ceil(a, c))
  let result2 = add(divide_with_ceil(multiply(b, c), a), subtract(b, c))
  
  assert_eq(55, result1)  // 15*4 - ceil(15/3) = 60 - 5 = 55
  assert_eq(5, result2)   // ceil(4*3/15) + (4-3) = 1 + 1 = 2
}

test "greet_function_with_spaces" {
  // 测试问候函数对空格的处理
  assert_eq_string("Hello,  leading space!", greet(" leading space"))
  assert_eq_string("Hello, trailing space !", greet("trailing space "))
  assert_eq_string("Hello,  multiple  spaces !", greet("  multiple  spaces "))
}

test "divide_with_ceil_precision_edge" {
  // 测试向上取整除法的精度边缘情况
  assert_eq(1, divide_with_ceil(2, 2))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(1001, divide_with_ceil(2001, 2))
  assert_eq(500, divide_with_ceil(999, 2))
}

test "mathematical_identity_extended" {
  // 测试扩展的数学恒等式
  let x = 42
  
  // 加法恒等式
  assert_eq(x, add(x, 0))
  assert_eq(0, subtract(x, x))
  
  // 乘法恒等式
  assert_eq(x, multiply(x, 1))
  assert_eq(0, multiply(x, 0))
  assert_eq(x, divide_with_ceil(multiply(x, 1), 1))
}

test "negative_zero_handling" {
  // 测试负零的处理
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, -1))
  assert_eq(0, divide_with_ceil(0, -1))
  assert_eq(0, subtract(0, 0))
}

test "large_number_division" {
  // 测试大数除法
  assert_eq(100000, divide_with_ceil(100000000, 1000))
  assert_eq(100001, divide_with_ceil(100000001, 1000))
  assert_eq(1, divide_with_ceil(1000000, 1000000))
  assert_eq(2, divide_with_ceil(1000001, 1000000))
}

test "string_concatenation_edge" {
  // 测试字符串连接的边缘情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, !", greet(" "))
  assert_eq_string("Hello, a!", greet("a"))
  assert_eq_string("Hello, abcdefghijklmnopqrstuvwxyz!", greet("abcdefghijklmnopqrstuvwxyz"))
}

test "sequential_calculation_pipeline" {
  // 测试连续计算流水线
  let initial = 100
  let step1 = divide_with_ceil(initial, 3)      // 34
  let step2 = multiply(step1, 2)                // 68
  let step3 = subtract(step2, 15)               // 53
  let step4 = add(step3, divide_with_ceil(10, 3)) // 53 + 4 = 57
  
  assert_eq(57, step4)
}

test "resource_allocation_realistic" {
  // 真实资源分配场景测试
  // 场景：计算会议室需求
  let attendees = 127
  let room_capacity = 30
  let rooms_needed = divide_with_ceil(attendees, room_capacity)
  
  assert_eq(5, rooms_needed)  // ceil(127/30) = 5
  
  // 计算总座位数
  let total_seats = multiply(rooms_needed, room_capacity)
  assert_eq(150, total_seats)
  
  // 计算空余座位
  let empty_seats = subtract(total_seats, attendees)
  assert_eq(23, empty_seats)
}
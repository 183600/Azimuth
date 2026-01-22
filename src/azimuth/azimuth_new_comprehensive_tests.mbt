// Azimuth 综合测试套件
// 包含10个高质量的MoonBit测试用例，覆盖新的测试场景

test "fibonacci_sequence_calculation" {
  // 测试斐波那契数列计算
  let fib1 = 1
  let fib2 = 1
  let fib3 = add(fib1, fib2)
  let fib4 = add(fib2, fib3)
  let fib5 = add(fib3, fib4)
  let fib6 = add(fib4, fib5)
  let fib7 = add(fib5, fib6)
  let fib8 = add(fib6, fib7)
  
  assert_eq(2, fib3)
  assert_eq(3, fib4)
  assert_eq(5, fib5)
  assert_eq(8, fib6)
  assert_eq(13, fib7)
  assert_eq(21, fib8)
}

test "factorial_calculation" {
  // 测试阶乘计算
  let fact1 = 1
  let fact2 = multiply(fact1, 2)
  let fact3 = multiply(fact2, 3)
  let fact4 = multiply(fact3, 4)
  let fact5 = multiply(fact4, 5)
  
  assert_eq(120, fact5)
}

test "stack_operations_simulation" {
  // 模拟栈操作（push和pop）
  let mut stack_size = 0
  let mut stack_sum = 0
  
  // push操作
  stack_size = add(stack_size, 1)
  stack_sum = add(stack_sum, 10)
  stack_size = add(stack_size, 1)
  stack_sum = add(stack_sum, 20)
  stack_size = add(stack_size, 1)
  stack_sum = add(stack_sum, 30)
  
  assert_eq(3, stack_size)
  assert_eq(60, stack_sum)
  
  // pop操作（模拟）
  stack_size = add(stack_size, -1)
  stack_sum = add(stack_sum, -30)
  
  assert_eq(2, stack_size)
  assert_eq(30, stack_sum)
}

test "binary_search_efficiency" {
  // 模拟二分搜索的比较次数
  let sorted_array_size = 64
  let mut binary_comparisons = divide_with_ceil(sorted_array_size, 2)
  binary_comparisons = divide_with_ceil(binary_comparisons, 2)
  binary_comparisons = divide_with_ceil(binary_comparisons, 2)
  binary_comparisons = divide_with_ceil(binary_comparisons, 2)
  binary_comparisons = divide_with_ceil(binary_comparisons, 2)
  
  assert_eq(1, binary_comparisons)
}

test "boundary_value_calculations" {
  // 测试除法的各种边界情况
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(1, 0))
  assert_eq(0, divide_with_ceil(-1, 0))
  
  // 测试极值运算
  let very_large = 1000000
  let negative_large = -1000000
  
  assert_eq(0, add(very_large, negative_large))
}

test "multilingual_greetings" {
  // 测试多语言问候功能
  assert_eq_string("Hello, English!", greet("English"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 日本語!", greet("日本語"))
  assert_eq_string("Hello, 한국어!", greet("한국어"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
  assert_eq_string("Hello, русский!", greet("русский"))
  assert_eq_string("Hello, español!", greet("español"))
  assert_eq_string("Hello, français!", greet("français"))
}

test "physics_distance_calculation" {
  // 计算自由落体距离：s = 0.5 * g * t^2（简化为整数计算）
  let gravity = 10  // 简化的重力加速度
  let time = 3
  let distance = multiply(gravity, multiply(time, time)) / 2
  
  assert_eq(45, distance)
}

test "kinetic_energy_calculation" {
  // 计算动能：KE = 0.5 * m * v^2
  let mass = 10
  let velocity = 8
  let kinetic_energy = multiply(mass, multiply(velocity, velocity)) / 2
  
  assert_eq(320, kinetic_energy)
}

test "circle_geometry_calculations" {
  // 计算圆的周长和面积
  let radius = 7
  let pi_approx = 22  // π的近似值
  let circumference = multiply(2, multiply(radius, pi_approx)) / 7
  let area = multiply(radius, multiply(radius, pi_approx)) / 7
  
  assert_eq(44, circumference)
  assert_eq(154, area)
}

test "combinatorial_calculations" {
  // 计算组合数：C(n,k) = n! / (k! * (n-k)!)
  // 简化计算：C(5,2) = 5*4/(2*1) = 10
  let n = 5
  let k = 2
  let numerator = multiply(n, add(n, -1))
  let denominator = multiply(k, 1)
  let combination = numerator / denominator
  
  assert_eq(10, combination)
  
  // 计算排列数：P(n,k) = n! / (n-k)!
  // 简化计算：P(5,3) = 5*4*3 = 60
  let perm_n = 5
  let permutation = multiply(perm_n, multiply(add(perm_n, -1), add(perm_n, -2)))
  
  assert_eq(60, permutation)
}
// 增强的 MoonBit 测试用例
// 这些测试用例使用标准的 MoonBit 测试语法，覆盖 azimuth 模块的核心功能

// 导入主模块中的函数
use azimuth

test "divide_with_ceil_basic_functionality" {
  // 测试基本的向上取整除法功能
  assert_eq(3, divide_with_ceil(10, 3))
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(5, divide_with_ceil(15, 3))
  assert_eq(2, divide_with_ceil(4, 2))
  assert_eq(1, divide_with_ceil(1, 1))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(1, divide_with_ceil(1, 5))
  assert_eq(5, divide_with_ceil(25, 5))
  assert_eq(6, divide_with_ceil(26, 5))
  
  // 测试负数情况
  assert_eq(-2, divide_with_ceil(-10, 5))
  assert_eq(-1, divide_with_ceil(-1, 5))
}

test "complex_arithmetic_combinations" {
  // 测试复杂的算术组合
  let a = 12
  let b = 8
  let c = 5
  
  // (a + b) * c - (a - b)
  let result = add(multiply(add(a, b), c), add(a, -b))
  assert_eq(92, result)
  
  // a * b + b * c - a * c
  let result2 = add(add(multiply(a, b), multiply(b, c)), multiply(-a, c))
  assert_eq(76, result2)
}

test "string_processing_advanced" {
  // 测试高级字符串处理
  // 测试包含各种特殊字符的字符串
  let special_chars = "!@#$%^&*()"
  assert_eq_string("Hello, !@#$%^&*()!", greet(special_chars))
  
  // 测试包含Unicode字符的字符串
  let unicode_chars = "🌟✨🎉🚀"
  assert_eq_string("Hello, 🌟✨🎉🚀!", greet(unicode_chars))
  
  // 测试包含多种语言的字符串
  let multilingual = "Hello世界こんにちは안녕하세요"
  assert_eq_string("Hello, Hello世界こんにちは안녕하세요!", greet(multilingual))
}

test "mathematical_series_and_sequences" {
  // 测试数学级数和序列
  // 计算斐波那契数列的前几项和：1, 1, 2, 3, 5, 8
  let fib1 = 1
  let fib2 = 1
  let fib3 = add(fib1, fib2)  // 2
  let fib4 = add(fib2, fib3)  // 3
  let fib5 = add(fib3, fib4)  // 5
  let fib6 = add(fib4, fib5)  // 8
  
  let sum_fib = add(add(add(add(add(fib1, fib2), fib3), fib4), fib5), fib6)
  assert_eq(20, sum_fib)
  
  // 计算等比数列的和：2 + 4 + 8 + 16 = 30
  let geo1 = 2
  let geo2 = multiply(geo1, 2)  // 4
  let geo3 = multiply(geo2, 2)  // 8
  let geo4 = multiply(geo3, 2)  // 16
  
  let sum_geo = add(add(add(geo1, geo2), geo3), geo4)
  assert_eq(30, sum_geo)
}

test "real_world_financial_calculations" {
  // 测试实际金融计算场景
  // 计算贷款月供（简化版）：本金 / 期数 + 利息
  let principal = 12000
  let months = 12
  let monthly_rate = 1  // 1%
  
  let monthly_principal = principal / months
  let first_month_interest = multiply(principal, monthly_rate) / 100
  let first_month_payment = add(monthly_principal, first_month_interest)
  
  assert_eq(1100, first_month_payment)
  
  // 计算复利（简化版）
  let initial = 1000
  let rate = 5
  let years = 3
  
  let yearly_growth = multiply(initial, rate) / 100
  let total = add(initial, multiply(yearly_growth, years))
  assert_eq(1150, total)
}

test "geometry_calculations" {
  // 测试几何计算
  // 计算直角三角形的斜边（勾股定理简化版）
  let base = 3
  let height = 4
  
  // c² = a² + b²
  let base_squared = multiply(base, base)
  let height_squared = multiply(height, height)
  let hypotenuse_squared = add(base_squared, height_squared)
  
  // 简化计算：√25 ≈ 5
  assert_eq(25, hypotenuse_squared)
  
  // 计算梯形面积：(上底 + 下底) × 高 ÷ 2
  let top_base = 6
  let bottom_base = 10
  let trapezoid_height = 4
  
  let area = multiply(add(top_base, bottom_base), trapezoid_height) / 2
  assert_eq(32, area)
}

test "algorithmic_problem_solving" {
  // 测试算法问题求解
  // 计算最大公约数（欧几里得算法简化版）
  let a = 48
  let b = 18
  
  // 简化的GCD计算：通过连续减法
  let diff = add(a, -b)  // 30
  let diff2 = add(diff, -b)  // 12
  let diff3 = add(b, -diff2)  // 6
  let diff4 = add(diff2, multiply(-2, diff3))  // 0
  
  // 当余数为0时，除数就是GCD
  assert_eq(6, diff3)
  
  // 计算最小公倍数：LCM(a,b) = |a × b| / GCD(a,b)
  let lcm = multiply(a, b) / diff3
  assert_eq(144, lcm)
}

test "data_structure_operations_simulation" {
  // 测试数据结构操作模拟
  // 模拟栈操作：push, push, pop, push
  let stack_size = 0
  
  // push 10
  stack_size = add(stack_size, 1)
  // push 20
  stack_size = add(stack_size, 1)
  // pop
  stack_size = add(stack_size, -1)
  // push 30
  stack_size = add(stack_size, 1)
  
  assert_eq(2, stack_size)
  
  // 模拟队列操作：enqueue, enqueue, dequeue, enqueue
  let queue_front = 0
  let queue_rear = 0
  
  // enqueue 10
  queue_rear = add(queue_rear, 1)
  // enqueue 20
  queue_rear = add(queue_rear, 1)
  // dequeue
  queue_front = add(queue_front, 1)
  // enqueue 30
  queue_rear = add(queue_rear, 1)
  
  let queue_size = add(queue_rear, -queue_front)
  assert_eq(2, queue_size)
}

test "comprehensive_boundary_testing" {
  // 综合边界值测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试加法的各种边界组合
  assert_eq(max_val, add(max_val, 0))
  assert_eq(max_val, add(0, max_val))
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(1, max_val))
  
  assert_eq(min_val, add(min_val, 0))
  assert_eq(min_val, add(0, min_val))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(-1, min_val))
  
  assert_eq(-1, add(max_val, min_val))
  assert_eq(-1, add(min_val, max_val))
  
  // 测试乘法的各种边界组合
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(0, max_val))
  assert_eq(0, multiply(min_val, 0))
  assert_eq(0, multiply(0, min_val))
  
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(max_val, multiply(1, max_val))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(1, min_val))
  
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(min_val, multiply(-1, min_val))
  assert_eq(min_val, multiply(max_val, -1))
  assert_eq(min_val, multiply(-1, max_val))
  
  // 测试复杂的边界运算组合
  let complex_boundary = add(multiply(add(max_val, min_val), 1), max_val)
  assert_eq(max_val, complex_boundary)
}
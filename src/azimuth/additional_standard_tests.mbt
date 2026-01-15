// 额外的标准 MoonBit 测试用例
// 这些测试用例覆盖了更多的边界情况和实际应用场景

test "division_simulation" {
  // 使用减法和计数来模拟整数除法
  let dividend = 27
  let divisor = 4
  
  // 手动计算商和余数
  let quotient = dividend / divisor
  let remainder = dividend % divisor
  
  assert_eq(6, quotient)
  assert_eq(3, remainder)
  
  // 验证：dividend = divisor * quotient + remainder
  let verification = add(multiply(divisor, quotient), remainder)
  assert_eq(dividend, verification)
}

test "modulo_properties" {
  // 测试模运算的性质
  let a = 17
  let b = 5
  
  // 基本模运算
  let remainder = a % b
  assert_eq(2, remainder)
  
  // 性质：a = (a / b) * b + (a % b)
  let quotient = a / b
  let verification = add(multiply(quotient, b), remainder)
  assert_eq(a, verification)
  
  // 负数模运算
  let negative_a = -17
  let negative_remainder = negative_a % b
  assert_eq(-2, negative_remainder)
}

test "string_processing_advanced" {
  // 高级字符串处理测试
  let first_name = "John"
  let last_name = "Doe"
  let age = "30"
  
  // 使用greet函数进行字符串拼接
  let full_greeting = greet(first_name + " " + last_name)
  assert_eq_string("Hello, John Doe!", full_greeting)
  
  // 测试包含数字的字符串
  let age_greeting = greet("Age: " + age)
  assert_eq_string("Hello, Age: 30!", age_greeting)
  
  // 测试特殊字符组合
  let special_chars = "@#$%^&*()"
  let special_greeting = greet(special_chars)
  assert_eq_string("Hello, @#$%^&*()!", special_greeting)
}

test "mathematical_sequences" {
  // 数学序列测试
  // 斐波那契数列前几项：1, 1, 2, 3, 5, 8, 13, 21...
  let fib1 = 1
  let fib2 = 1
  let fib3 = add(fib1, fib2)  // 2
  let fib4 = add(fib2, fib3)  // 3
  let fib5 = add(fib3, fib4)  // 5
  let fib6 = add(fib4, fib5)  // 8
  
  assert_eq(2, fib3)
  assert_eq(3, fib4)
  assert_eq(5, fib5)
  assert_eq(8, fib6)
  
  // 验证斐波那契性质：fib(n) = fib(n-1) + fib(n-2)
  assert_eq(fib6, add(fib5, fib4))
}

test "calculator_simulation" {
  // 模拟简单计算器功能
  let x = 15
  let y = 7
  
  // 基本运算
  let sum = add(x, y)
  let product = multiply(x, y)
  let difference = add(x, -y)
  let quotient = x / y
  let remainder = x % y
  
  assert_eq(22, sum)
  assert_eq(105, product)
  assert_eq(8, difference)
  assert_eq(2, quotient)
  assert_eq(1, remainder)
  
  // 复合运算：(x + y) * (x - y)
  let composite = multiply(sum, difference)
  assert_eq(176, composite)  // 22 * 8 = 176
}

test "boundary_stress_test" {
  // 边界压力测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 连续边界运算
  let step1 = add(max_val, -1000000)  // 接近最大值
  let step2 = add(step1, 500000)      // 回到安全范围
  let step3 = multiply(step2, 2)      // 可能溢出
  let step4 = add(step3, -1000000)    // 调整
  
  assert_true(step1 > 2146483647)
  assert_true(step2 < max_val)
  assert_eq(max_val, step3)  // 应该溢出保护
  assert_true(step4 < max_val)
}

test "logical_operations_simulation" {
  // 模拟逻辑运算
  let a = 5
  let b = 10
  let c = 15
  
  // 逻辑与模拟：a < b AND b < c
  let condition1 = a < b
  let condition2 = b < c
  let logical_and = if condition1 && condition2 { 1 } else { 0 }
  assert_eq(1, logical_and)
  
  // 逻辑或模拟：a > b OR b < c
  let condition3 = a > b
  let condition4 = b < c
  let logical_or = if condition3 || condition4 { 1 } else { 0 }
  assert_eq(1, logical_or)
  
  // 逻辑非模拟
  let logical_not = if condition1 { 0 } else { 1 }
  assert_eq(0, logical_not)
}

test "data_structure_simulation" {
  // 模拟简单数据结构操作
  // 模拟数组访问：arr[i] = base + i * stride
  let base = 1000
  let stride = 10
  let index1 = 0
  let index2 = 3
  let index3 = 7
  
  let value1 = add(base, multiply(index1, stride))
  let value2 = add(base, multiply(index2, stride))
  let value3 = add(base, multiply(index3, stride))
  
  assert_eq(1000, value1)
  assert_eq(1030, value2)
  assert_eq(1070, value3)
  
  // 模拟链表遍历求和
  let node1 = 10
  let node2 = 20
  let node3 = 30
  let node4 = 40
  
  let list_sum = add(add(add(node1, node2), node3), node4)
  assert_eq(100, list_sum)
}

test "algorithm_complexity_simulation" {
  // 模拟算法复杂度计算
  let n = 100
  
  // O(1) 常数时间
  let constant_time = 42
  
  // O(n) 线性时间：求和 1 + 2 + ... + n
  let linear_sum = multiply(n, add(n, 1)) / 2
  assert_eq(5050, linear_sum)
  
  // O(n²) 平方时间：嵌套循环求和（简化版）
  let quadratic_sum = multiply(n, multiply(add(n, 1), add(multiply(2, n), 1))) / 6
  assert_eq(338350, quadratic_sum)
  
  // 验证复杂度关系
  assert_true(linear_sum < quadratic_sum)
  assert_true(constant_time < linear_sum)
}

test "real_world_physics_simulation" {
  // 实际物理场景模拟
  // 自由落体运动：h = v₀t + 0.5gt²（简化为整数计算）
  let initial_velocity = 10  // m/s
  let gravity = 10           // m/s²（简化值）
  let time = 5               // s
  
  // 计算位移：h = 10*5 + 0.5*10*5² = 50 + 125 = 175
  let velocity_component = multiply(initial_velocity, time)
  let gravity_component = multiply(gravity, multiply(time, time)) / 2
  let displacement = add(velocity_component, gravity_component)
  
  assert_eq(175, displacement)
  
  // 计算末速度：v = v₀ + gt = 10 + 10*5 = 60
  let final_velocity = add(initial_velocity, multiply(gravity, time))
  assert_eq(60, final_velocity)
}
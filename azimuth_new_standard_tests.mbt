// 新增的标准 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "add_commutative_property" {
  // 测试加法交换律: a + b = b + a
  let a = 15
  let b = 27
  assert_eq(add(a, b), add(b, a))
  
  let x = -10
  let y = 25
  assert_eq(add(x, y), add(y, x))
}

test "multiply_distributive_property" {
  // 测试乘法分配律: a * (b + c) = a * b + a * c
  let a = 4
  let b = 6
  let c = 3
  let left = multiply(a, add(b, c))
  let right = add(multiply(a, b), multiply(a, c))
  assert_eq(left, right)
}

test "divide_with_ceil_fractional_cases" {
  // 测试向上取整除法的小数情况
  assert_eq(3, divide_with_ceil(8, 3))    // 8/3 = 2.666... -> 3
  assert_eq(7, divide_with_ceil(20, 3))   // 20/3 = 6.666... -> 7
  assert_eq(2, divide_with_ceil(4, 2))    // 4/2 = 2 -> 2
  assert_eq(1, divide_with_ceil(2, 3))    // 2/3 = 0.666... -> 1
}

test "greet_function_whitespace_handling" {
  // 测试问候函数的空白字符处理
  assert_eq_string("Hello,   World   !", greet("   World   "))
  assert_eq_string("Hello, 	\n!", greet("\t\n"))
  assert_eq_string("Hello, a b c!", greet("a b c"))
}

test "complex_calculation_pipeline" {
  // 测试复杂计算流水线
  let initial_value = 100
  let step1 = add(initial_value, multiply(5, 4))     // 100 + 20 = 120
  let step2 = multiply(step1, divide_with_ceil(25, 5)) // 120 * 5 = 600
  let final_result = add(step2, -multiply(10, 6))     // 600 - 60 = 540
  assert_eq(540, final_result)
}

test "edge_case_zero_division" {
  // 测试零除法的边界情况
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  assert_eq(0, divide_with_ceil(0, 100))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(0, divide_with_ceil(-5, 0))
}

test "negative_multiply_patterns" {
  // 测试负数乘法模式
  assert_eq(50, multiply(-5, -10))     // 负负得正
  assert_eq(-30, multiply(5, -6))      // 正负得负
  assert_eq(-42, multiply(-7, 6))      // 负正得负
  assert_eq(0, multiply(-8, 0))        // 零元素性质
}

test "string_concatenation_with_greet" {
  // 测试字符串连接与问候函数的组合
  let prefix = "Dear"
  let name = "MoonBit"
  let suffix = "Community"
  let full_name = prefix + " " + name + " " + suffix
  let formal_greeting = greet(full_name)
  assert_eq_string("Hello, Dear MoonBit Community!", formal_greeting)
}

test "mathematical_sequence_sum" {
  // 测试数学序列求和
  // 计算 1 + 2 + 3 + ... + 10 的和
  let n = 10
  let sum = divide_with_ceil(multiply(n, add(n, 1)), 2)  // n*(n+1)/2 公式
  assert_eq(55, sum)
  
  // 验证通过循环方式（使用函数组合）
  let manual_sum = add(add(add(add(add(1, 2), add(3, 4)), add(5, 6)), add(7, 8)), add(9, 10))
  assert_eq(sum, manual_sum)
}

test "business_inventory_calculation" {
  // 测试业务库存计算
  let boxes_per_pallet = 12
  let items_per_box = 24
  let total_items = 350
  
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  let pallets_needed = divide_with_ceil(boxes_needed, boxes_per_pallet)
  
  assert_eq(15, boxes_needed)     // ceil(350/24) = 15
  assert_eq(2, pallets_needed)    // ceil(15/12) = 2
  
  // 计算总容量
  let total_capacity = multiply(pallets_needed, multiply(boxes_per_pallet, items_per_box))
  assert_eq(576, total_capacity)  // 2 * 12 * 24 = 576
}
// Azimuth 项目标准 MoonBit 测试用例
// 包含 9 个高质量测试用例，使用标准 MoonBit 测试语法

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(5, add(2, 3))
  assert_eq(6, multiply(2, 3))
  assert_eq(2, subtract(5, 3))
  assert_eq(2, divide_with_ceil(5, 3))
  assert_eq(3, divide_with_ceil(9, 3))
}

test "string_greeting_functionality" {
  // 字符串问候功能测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 用户!", greet("用户"))
}

test "division_with_ceil_edge_cases" {
  // 向上取整除法边界情况测试
  // 正常情况
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 5))
  
  // 零值情况
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 负数情况
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let initial = 100
  let step1 = add(initial, 50)      // 150
  let step2 = multiply(step1, 2)    // 300
  let step3 = subtract(step2, 80)   // 220
  let step4 = divide_with_ceil(step3, 7)  // 32
  
  assert_eq(150, step1)
  assert_eq(300, step2)
  assert_eq(220, step3)
  assert_eq(32, step4)
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法单位元
  assert_eq(add(a, 0), a)
  
  // 乘法单位元
  assert_eq(multiply(a, 1), a)
  
  // 减法的逆运算
  assert_eq(add(subtract(a, b), b), a)
}

test "boundary_value_analysis" {
  // 边界值分析测试
  // 零值边界
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 100))
  assert_eq(0, subtract(0, 0))
  assert_eq(0, divide_with_ceil(0, 10))
  
  // 单位值边界
  assert_eq(1, add(1, 0))
  assert_eq(100, multiply(1, 100))
  assert_eq(1, subtract(1, 0))
  assert_eq(100, divide_with_ceil(100, 1))
  
  // 负单位边界
  assert_eq(0, add(-1, 1))
  assert_eq(-100, multiply(-1, 100))
  assert_eq(1, subtract(0, -1))
}

test "shopping_cart_calculation" {
  // 购物车计算场景测试
  let item1_price = 25
  let item1_qty = 3
  let item2_price = 15
  let item2_qty = 2
  
  // 计算小计
  let subtotal1 = multiply(item1_price, item1_qty)  // 75
  let subtotal2 = multiply(item2_price, item2_qty)  // 30
  let total = add(subtotal1, subtotal2)             // 105
  
  // 计算税费（10%，向上取整）
  let tax = divide_with_ceil(multiply(total, 10), 100)  // 11
  
  // 计算总价
  let grand_total = add(total, tax)                // 116
  
  assert_eq(75, subtotal1)
  assert_eq(30, subtotal2)
  assert_eq(105, total)
  assert_eq(11, tax)
  assert_eq(116, grand_total)
}

test "algorithm_simulation_binary_search" {
  // 二分查找算法模拟测试
  // 在有序数组 [1, 3, 5, 7, 9] 中查找 7
  
  let low = 0
  let high = 4
  let target = 7
  
  // 第一次二分
  let mid1 = divide_with_ceil(add(low, high), 2)  // 2
  let mid1_value = 5  // 假设中间值是5
  let new_low1 = add(mid1, 1)  // 5 < 7，查找右半部分
  
  // 第二次二分
  let mid2 = divide_with_ceil(add(new_low1, high), 2)  // 3
  let mid2_value = 7  // 找到目标值
  
  assert_eq(2, mid1)
  assert_eq(3, new_low1)
  assert_eq(3, mid2)
}

test "data_structure_stack_simulation" {
  // 栈数据结构操作模拟测试
  let stack_size = 0
  let top_element = 0
  
  // push(10)
  stack_size = add(stack_size, 1)  // 1
  top_element = 10
  
  // push(20)
  stack_size = add(stack_size, 1)  // 2
  top_element = 20
  
  // push(30)
  stack_size = add(stack_size, 1)  // 3
  top_element = 30
  
  // pop() -> 返回30
  let popped_value = top_element  // 30
  stack_size = subtract(stack_size, 1)  // 2
  top_element = 20  // 新栈顶
  
  // pop() -> 返回20
  let popped_value2 = top_element  // 20
  stack_size = subtract(stack_size, 1)  // 1
  top_element = 10  // 新栈顶
  
  assert_eq(30, popped_value)
  assert_eq(20, popped_value2)
  assert_eq(1, stack_size)
  assert_eq(10, top_element)
}
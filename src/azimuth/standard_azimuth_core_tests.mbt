// Azimuth 库的标准测试用例
// 测试核心功能：add, multiply, subtract, greet, divide_with_ceil

use azimuth

test "add_function_basic_operations" {
  // 测试加法函数的基本运算
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(10, add(15, -5))
  assert_eq(1000000, add(500000, 500000))
}

test "multiply_function_comprehensive" {
  // 测试乘法函数的综合运算
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 10))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(6, multiply(-2, -3))
  assert_eq(100, multiply(10, 10))
  assert_eq(1000000, multiply(1000, 1000))
}

test "subtract_function_edge_cases" {
  // 测试减法函数的边界情况
  assert_eq(5, subtract(8, 3))
  assert_eq(-5, subtract(3, 8))
  assert_eq(0, subtract(10, 10))
  assert_eq(10, subtract(0, -10))
  assert_eq(-10, subtract(-10, 0))
  assert_eq(20, subtract(-5, -25))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "divide_with_ceil_positive_numbers" {
  // 测试向上取整除法的正数情况
  assert_eq(4, divide_with_ceil(10, 3))  // 10/3 = 3.333... -> 4
  assert_eq(3, divide_with_ceil(9, 3))   // 9/3 = 3 -> 3
  assert_eq(1, divide_with_ceil(1, 100)) // 1/100 = 0.01 -> 1
  assert_eq(2, divide_with_ceil(3, 2))   // 3/2 = 1.5 -> 2
  assert_eq(7, divide_with_ceil(13, 2))  // 13/2 = 6.5 -> 7
}

test "divide_with_ceil_negative_numbers" {
  // 测试向上取整除法的负数情况
  assert_eq(-3, divide_with_ceil(-10, 3))   // -10/3 = -3.333... -> -3
  assert_eq(4, divide_with_ceil(-10, -3))   // -10/-3 = 3.333... -> 4
  assert_eq(-2, divide_with_ceil(-4, 2))    // -4/2 = -2 -> -2
  assert_eq(0, divide_with_ceil(5, 0))      // 除数为零时返回0
  assert_eq(0, divide_with_ceil(-5, 0))     // 除数为零时返回0
}

test "mathematical_properties_validation" {
  // 测试数学性质验证
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 零元素性质
  assert_eq(0, multiply(a, 0))
  assert_eq(a, add(a, 0))
  
  // 减法的逆运算
  assert_eq(a, add(subtract(a, b), b))
}

test "complex_business_calculations" {
  // 测试复杂业务计算场景
  // 计算订单总价：商品价格 × 数量 + 运费 - 折扣
  let unit_price = 25
  let quantity = 8
  let shipping = 15
  let discount = 20
  
  let subtotal = multiply(unit_price, quantity)
  let total_before_discount = add(subtotal, shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(200, subtotal)
  assert_eq(215, total_before_discount)
  assert_eq(195, final_total)
}

test "packaging_and_resource_allocation" {
  // 测试包装和资源分配计算
  let total_items = 47
  let items_per_box = 12
  
  // 计算需要的包装箱数量
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  // 计算总容量和剩余空间
  let total_capacity = multiply(boxes_needed, items_per_box)
  let remaining_space = subtract(total_capacity, total_items)
  
  assert_eq(4, boxes_needed)        // ceil(47/12) = 4
  assert_eq(48, total_capacity)     // 4 * 12 = 48
  assert_eq(1, remaining_space)     // 48 - 47 = 1
}

test "error_handling_and_boundary_values" {
  // 测试错误处理和边界值
  // 测试零值运算
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(5, add(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  
  // 测试大数运算
  let big_num = 1000000
  assert_eq(2000000, add(big_num, big_num))
  assert_eq(1000000000000, multiply(big_num, big_num))
  
  // 测试极端边界
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}
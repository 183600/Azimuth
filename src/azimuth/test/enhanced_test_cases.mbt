// Azimuth 项目增强测试用例
// 覆盖高级场景和实际应用案例

test "stack_operations_simulation" {
  // 模拟栈操作：使用加减法模拟push和pop
  let mut stack_size = 0
  
  // Push操作
  stack_size = @azimuth.add(stack_size, 1)
  stack_size = @azimuth.add(stack_size, 1)
  stack_size = @azimuth.add(stack_size, 1)
  
  @azimuth.assert_eq(3, stack_size)
  
  // Pop操作
  stack_size = @azimuth.add(stack_size, -1)
  stack_size = @azimuth.add(stack_size, -1)
  stack_size = @azimuth.add(stack_size, -1)
  
  @azimuth.assert_eq(0, stack_size)
}

test "binary_search_simulation" {
  // 模拟二分查找：在有序数组中查找元素
  let target = 40
  let mut iterations = 0
  
  // 简化的二分查找模拟
  let mut low = 0
  let mut high = 9
  let mut mid = @azimuth.add(low, @azimuth.multiply(@azimuth.add(high, -low), 5) / 10)
  
  // 第一次查找
  iterations = @azimuth.add(iterations, 1)
  if (target < 50) {
    high = @azimuth.add(mid, -1)
  } else {
    low = @azimuth.add(mid, 1)
  }
  
  // 第二次查找
  mid = @azimuth.add(low, @azimuth.multiply(@azimuth.add(high, -low), 5) / 10)
  iterations = @azimuth.add(iterations, 1)
  if (target > 20) {
    low = @azimuth.add(mid, 1)
  } else {
    high = @azimuth.add(mid, -1)
  }
  
  // 第三次查找
  mid = @azimuth.add(low, @azimuth.multiply(@azimuth.add(high, -low), 5) / 10)
  iterations = @azimuth.add(iterations, 1)
  if (target > 30) {
    low = @azimuth.add(mid, 1)
  } else {
    high = @azimuth.add(mid, -1)
  }
  
  // 第四次查找
  mid = @azimuth.add(low, @azimuth.multiply(@azimuth.add(high, -low), 5) / 10)
  iterations = @azimuth.add(iterations, 1)
  
  @azimuth.assert_eq(3, mid)
  @azimuth.assert_eq(4, iterations)
}

test "string_special_characters" {
  // 测试字符串包含特殊字符的处理
  let special_string = "Path/to/file.txt"
  let special_greeting = @azimuth.greet(special_string)
  @azimuth.assert_eq_string("Hello, Path/to/file.txt!", special_greeting)
  
  // 测试包含数字和字母的混合字符串
  let mixed_string = "User2023"
  let mixed_greeting = @azimuth.greet(mixed_string)
  @azimuth.assert_eq_string("Hello, User2023!", mixed_greeting)
}

test "derivative_approximation" {
  // 微积分导数近似计算：f(x) = x²，在x=3处的导数
  let x = 3
  let h = 1
  
  // 计算 f(x) = x²
  let fx = @azimuth.multiply(x, x)
  
  // 计算 f(x+h) = (x+h)²
  let x_plus_h = @azimuth.add(x, h)
  let fx_plus_h = @azimuth.multiply(x_plus_h, x_plus_h)
  
  // 计算导数近似值
  let derivative_approx = @azimuth.add(fx_plus_h, -fx) / h
  
  @azimuth.assert_eq(7, derivative_approx)
}

test "overflow_comprehensive" {
  // 综合溢出测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试加法溢出
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  
  // 测试乘法溢出
  @azimuth.assert_eq(max_val, @azimuth.multiply(100000, 30000))
  @azimuth.assert_eq(min_val, @azimuth.multiply(-100000, 30000))
  
  // 测试边界值组合
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
}

test "fibonacci_sequence" {
  // 斐波那契数列计算
  let f0 = 0
  let f1 = 1
  let f2 = @azimuth.add(f1, f0)
  let f3 = @azimuth.add(f2, f1)
  let f4 = @azimuth.add(f3, f2)
  let f5 = @azimuth.add(f4, f3)
  let f6 = @azimuth.add(f5, f4)
  let f7 = @azimuth.add(f6, f5)
  let f8 = @azimuth.add(f7, f6)
  let f9 = @azimuth.add(f8, f7)
  let f10 = @azimuth.add(f9, f8)
  
  @azimuth.assert_eq(0, f0)
  @azimuth.assert_eq(1, f1)
  @azimuth.assert_eq(1, f2)
  @azimuth.assert_eq(2, f3)
  @azimuth.assert_eq(3, f4)
  @azimuth.assert_eq(5, f5)
  @azimuth.assert_eq(8, f6)
  @azimuth.assert_eq(13, f7)
  @azimuth.assert_eq(21, f8)
  @azimuth.assert_eq(34, f9)
  @azimuth.assert_eq(55, f10)
}

test "mathematical_series" {
  // 数学级数计算
  let n = 100
  
  // 使用公式：sum = n × (n + 1) / 2
  let formula_sum = @azimuth.multiply(n, @azimuth.add(n, 1)) / 2
  @azimuth.assert_eq(5050, formula_sum)
  
  // 几何级数求和：2^0 + 2^1 + 2^2 + ... + 2^k
  let power_sum = @azimuth.add(@azimuth.multiply(@azimuth.multiply(@azimuth.multiply(2, 2), 2), 2), -1)
  @azimuth.assert_eq(31, power_sum)
}

test "inventory_management" {
  // 库存管理系统测试
  let product_a_stock = 100
  let product_a_sales = 30
  let product_a_restock = 50
  
  // 计算销售后库存
  let product_a_after_sales = @azimuth.add(product_a_stock, -product_a_sales)
  
  // 计算补货后库存
  let product_a_final = @azimuth.add(product_a_after_sales, product_a_restock)
  
  @azimuth.assert_eq(120, product_a_final)
  
  // 计算库存价值
  let price_a = 10
  let value_a = @azimuth.multiply(product_a_final, price_a)
  @azimuth.assert_eq(1200, value_a)
}
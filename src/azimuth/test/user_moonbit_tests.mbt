// 用户自定义 MoonBit 测试用例
// 为 Azimuth 项目添加的8个标准测试用例

test "sequential_calculation_chain" {
  // 测试连续计算链：((a + b) * c - d) / e
  let a = 8
  let b = 12
  let c = 3
  let d = 15
  let e = 5
  
  // 逐步计算
  let sum_ab = @azimuth.add(a, b)  // 8 + 12 = 20
  let product = @azimuth.multiply(sum_ab, c)  // 20 * 3 = 60
  let difference = @azimuth.add(product, -d)  // 60 - 15 = 45
  let result = difference / e  // 45 / 5 = 9
  
  @azimuth.assert_eq(9, result)
}

test "string_processing_with_unicode" {
  // 测试包含 Unicode 字符的字符串处理
  let chinese_name = "李明"
  let japanese_name = "田中"
  let emoji_name = "🚀"
  let mixed_name = "User2023@测试"
  
  @azimuth.assert_eq_string("Hello, 李明!", @azimuth.greet(chinese_name))
  @azimuth.assert_eq_string("Hello, 田中!", @azimuth.greet(japanese_name))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet(emoji_name))
  @azimuth.assert_eq_string("Hello, User2023@测试!", @azimuth.greet(mixed_name))
}

test "mathematical_series_calculations" {
  // 测试数学级数计算
  // 计算斐波那契数列前10项的和：1, 1, 2, 3, 5, 8, 13, 21, 34, 55
  let fib1 = 1
  let fib2 = 1
  let fib3 = 2
  let fib4 = 3
  let fib5 = 5
  let fib6 = 8
  let fib7 = 13
  let fib8 = 21
  let fib9 = 34
  let fib10 = 55
  
  let fib_sum = @azimuth.add(
    @azimuth.add(
      @azimuth.add(
        @azimuth.add(
          @azimuth.add(fib1, fib2),
          @azimuth.add(fib3, fib4)
        ),
        @azimuth.add(fib5, fib6)
      ),
      @azimuth.add(fib7, fib8)
    ),
    @azimuth.add(fib9, fib10)
  )
  
  @azimuth.assert_eq(143, fib_sum)
}

test "business_calculation_scenario" {
  // 测试业务计算场景：订单折扣计算
  let item_price = 199
  let quantity = 5
  let discount_threshold = 500
  let discount_rate = 15
  let tax_rate = 8
  let shipping = 25
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(item_price, quantity)
  
  // 计算折扣（如果超过阈值）
  let discount = if (subtotal > discount_threshold) {
    @azimuth.multiply(subtotal, discount_rate) / 100
  } else {
    0
  }
  
  // 计算折后价格
  let discounted_total = @azimuth.add(subtotal, -discount)
  
  // 计算税费
  let tax = @azimuth.multiply(discounted_total, tax_rate) / 100
  
  // 计算最终总价
  let final_total = @azimuth.add(@azimuth.add(discounted_total, tax), shipping)
  
  @azimuth.assert_eq(976, final_total)
}

test "extreme_boundary_combinations" {
  // 测试极端边界值组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与最小值的各种运算组合
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  
  // 测试连续边界运算
  let chain1 = @azimuth.add(@azimuth.multiply(max_val, 1), min_val)
  @azimuth.assert_eq(-1, chain1)
  
  let chain2 = @azimuth.multiply(@azimuth.add(min_val, 1), -1)
  @azimuth.assert_eq(2147483647, chain2)
}

test "scientific_calculations" {
  // 测试科学计算场景
  // 计算动能：E = 1/2 * m * v^2
  let mass = 10
  let velocity = 15
  let kinetic_energy = @azimuth.multiply(mass, @azimuth.multiply(velocity, velocity)) / 2
  @azimuth.assert_eq(1125, kinetic_energy)
  
  // 计算势能：E = m * g * h (g 简化为 10)
  let height = 20
  let gravity = 10
  let potential_energy = @azimuth.multiply(@azimuth.multiply(mass, gravity), height)
  @azimuth.assert_eq(2000, potential_energy)
  
  // 计算总能量
  let total_energy = @azimuth.add(kinetic_energy, potential_energy)
  @azimuth.assert_eq(3125, total_energy)
}

test "data_structure_simulation" {
  // 测试数据结构模拟：栈操作
  // 模拟栈的 push 和 pop 操作
  let stack1 = 10
  let stack2 = @azimuth.add(stack1, 20)  // push 20
  let stack3 = @azimuth.add(stack2, 30)  // push 30
  let stack4 = @azimuth.add(stack3, 40)  // push 40
  
  // 模拟 pop 操作（通过减法）
  let popped1 = @azimuth.add(stack4, -40)  // pop 40
  let popped2 = @azimuth.add(popped1, -30)  // pop 30
  let popped3 = @azimuth.add(popped2, -20)  // pop 20
  
  @azimuth.assert_eq(10, popped3)  // 最终剩下初始值
  
  // 测试队列操作模拟
  let queue_front = 5
  let queue_rear = @azimuth.add(queue_front, 15)  // enqueue 15
  let queue_rear2 = @azimuth.add(queue_rear, 25)  // enqueue 25
  
  // 模拟 dequeue 操作
  let dequeued = queue_front  // dequeue 5
  let new_front = @azimuth.add(queue_front, 15)  // 新的队头
  
  @azimuth.assert_eq(20, new_front)
}

test "error_boundary_validation" {
  // 测试错误边界验证
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 验证断言函数的正确性
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(100, 100)
  @azimuth.assert_eq_string("test", "test")
  
  // 验证溢出保护机制
  @azimuth.assert_true(@azimuth.add(max_val, 1) == max_val)
  @azimuth.assert_true(@azimuth.add(min_val, -1) == min_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
  @azimuth.assert_true(@azimuth.multiply(min_val, 2) == min_val)
  
  // 验证 divide_with_ceil 函数
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(17, 4))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(16, 4))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(-16, 4))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-11, 4))
}
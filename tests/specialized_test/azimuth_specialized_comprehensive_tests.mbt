// Azimuth 专业综合测试用例
// 专注于数据结构、算法和边界条件的测试

// 测试1: 数组栈操作模拟
test "数组栈操作模拟" {
  let stack = []
  
  // 测试空栈
  assert_eq(stack.length(), 0)
  
  // 模拟入栈操作
  stack = stack.push(1)
  stack = stack.push(2)
  stack = stack.push(3)
  assert_eq(stack.length(), 3)
  
  // 模拟出栈操作
  assert_eq(stack[stack.length() - 1], 3)
  stack = stack.slice(0, stack.length() - 1)
  assert_eq(stack[stack.length() - 1], 2)
  stack = stack.slice(0, stack.length() - 1)
  assert_eq(stack[stack.length() - 1], 1)
  stack = stack.slice(0, stack.length() - 1)
  assert_eq(stack.length(), 0)
}

// 测试2: 数组队列操作模拟
test "数组队列操作模拟" {
  let queue = []
  
  // 测试空队列
  assert_eq(queue.length(), 0)
  
  // 模拟入队操作
  queue = queue.push(1)
  queue = queue.push(2)
  queue = queue.push(3)
  assert_eq(queue.length(), 3)
  
  // 模拟出队操作 (FIFO)
  assert_eq(queue[0], 1)
  queue = queue.slice(1)
  assert_eq(queue[0], 2)
  queue = queue.slice(1)
  assert_eq(queue[0], 3)
  queue = queue.slice(1)
  assert_eq(queue.length(), 0)
}

// 测试3: 数组映射操作
test "数组映射操作" {
  let numbers = [1, 2, 3, 4, 5]
  
  // 测试映射操作
  let doubled = numbers.map(fn(x) { x * 2 })
  assert_eq(doubled, [2, 4, 6, 8, 10])
  
  // 测试过滤操作
  let evens = numbers.filter(fn(x) { x % 2 == 0 })
  assert_eq(evens, [2, 4])
  
  // 测试归约操作
  let sum = numbers.reduce(fn(acc, x) { acc + x }, 0)
  assert_eq(sum, 15)
}

// 测试4: 排序算法测试
test "基本排序算法" {
  let unsorted = [5, 2, 8, 1, 9, 3, 7, 4, 6]
  
  // 实现简单的冒泡排序
  let sorted = unsorted
  let n = sorted.length()
  for i = 0; i < n - 1; i = i + 1 {
    for j = 0; j < n - i - 1; j = j + 1 {
      if sorted[j] > sorted[j + 1] {
        let temp = sorted[j]
        sorted[j] = sorted[j + 1]
        sorted[j + 1] = temp
      }
    }
  }
  
  // 验证排序结果
  assert_eq(sorted[0], 1)
  assert_eq(sorted[1], 2)
  assert_eq(sorted[2], 3)
  assert_eq(sorted[3], 4)
  assert_eq(sorted[4], 5)
  assert_eq(sorted[5], 6)
  assert_eq(sorted[6], 7)
  assert_eq(sorted[7], 8)
  assert_eq(sorted[8], 9)
  
  // 测试空数组排序
  let empty = []
  assert_eq(empty.length(), 0)
  
  // 测试单元素数组排序
  let single = [5]
  assert_eq(single.length(), 1)
  assert_eq(single[0], 5)
}

// 测试5: 字符串处理
test "字符串处理操作" {
  let text = "Hello, Azimuth Telemetry System!"
  
  // 测试长度
  assert_eq(text.length(), 31)
  
  // 测试字符串连接
  let str1 = "Hello"
  let str2 = "World"
  let combined = str1 + ", " + str2
  assert_eq(combined, "Hello, World")
  
  // 测试大小写转换
  assert_eq(str1.to_uppercase(), "HELLO")
  assert_eq(str2.to_lowercase(), "world")
  
  // 测试字符串包含
  assert_true(text.contains("Azimuth"))
  assert_true(text.contains("Telemetry"))
  assert_false(text.contains("NotFound"))
}

// 测试6: 数值边界条件
test "数值边界条件处理" {
  // 测试基本算术运算
  assert_eq(2147483647 + 1, -2147483648) // 溢出测试
  assert_eq(-2147483648 - 1, 2147483647) // 下溢测试
  
  // 测试零值运算
  assert_eq(0 * 5, 0)
  assert_eq(0 + 10, 10)
  assert_eq(10 - 0, 10)
  
  // 测试除法
  assert_eq(10 / 2, 5)
  assert_eq(10 / 5, 2)
  assert_eq(1 / 2, 0) // 整数除法
  
  // 测试取模
  assert_eq(10 % 3, 1)
  assert_eq(10 % 5, 0)
  assert_eq(5 % 10, 5)
}

// 测试7: 逻辑运算测试
test "逻辑运算测试" {
  // 测试基本逻辑运算
  assert_true(true && true)
  assert_false(true && false)
  assert_false(false && true)
  assert_false(false && false)
  
  assert_true(true || true)
  assert_true(true || false)
  assert_true(false || true)
  assert_false(false || false)
  
  assert_true(!false)
  assert_false(!true)
  
  // 测试复杂逻辑表达式
  assert_true((true && false) || true)
  assert_false((true && false) && true)
  assert_true(true || (false && false))
}

// 测试8: 数组高级操作
test "数组高级操作" {
  let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  
  // 测试数组切片
  let slice1 = numbers.slice(0, 5)
  assert_eq(slice1, [1, 2, 3, 4, 5])
  
  let slice2 = numbers.slice(5)
  assert_eq(slice2, [6, 7, 8, 9, 10])
  
  // 测试数组查找
  let found = numbers.find(fn(x) { x > 5 })
  assert_eq(found, 6)
  
  // 测试数组所有元素满足条件
  assert_true(numbers.all(fn(x) { x > 0 }))
  assert_false(numbers.all(fn(x) { x > 5 }))
  
  // 测试数组任一元素满足条件
  assert_true(numbers.any(fn(x) { x > 5 }))
  assert_false(numbers.any(fn(x) { x > 10 }))
}

// 测试9: 字符串高级处理
test "字符串高级处理" {
  let words = ["Hello", "Azimuth", "Telemetry", "System"]
  
  // 测试字符串数组连接
  let sentence = words.reduce(fn(acc, word) { acc + " " + word }, "")
  assert_eq(sentence.trim(), "Hello Azimuth Telemetry System")
  
  // 测试字符串数组映射
  let upper_words = words.map(fn(w) { w.to_uppercase() })
  assert_eq(upper_words, ["HELLO", "AZIMUTH", "TELEMETRY", "SYSTEM"])
  
  // 测试字符串数组过滤
  let long_words = words.filter(fn(w) { w.length() > 5 })
  assert_eq(long_words, ["Azimuth", "Telemetry"])
}

// 测试10: 条件分支测试
test "条件分支测试" {
  // 测试if-else基本用法
  let x = 10
  let result = ""
  
  if x > 5 {
    result = "greater"
  } else if x < 5 {
    result = "less"
  } else {
    result = "equal"
  }
  assert_eq(result, "greater")
  
  // 测试嵌套条件
  let y = 15
  let category = ""
  
  if x > 0 {
    if y > 10 {
      category = "positive-large"
    } else {
      category = "positive-small"
    }
  } else {
    category = "non-positive"
  }
  assert_eq(category, "positive-large")
}
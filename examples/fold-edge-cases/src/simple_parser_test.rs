use marigold::m;
use marigold::marigold_impl::StreamExt;

#[tokio::main]
async fn main() {
    println!("Testing simple multi-function parsing...");
    
    // Test 1: Stream first, then struct + function with regular braces
    let result1 = m!(
        range(1, 3).return
        
        struct Container {
            value: u32
        }
        
        fn make_container() -> Container {
            Container { value: 0 }
        }
    ).await.collect::<Vec<i32>>().await;
    println!("Single function result: {:?}", result1);
    
    // Test 2: Struct + two functions (this should fail)
    let result2 = m!(
        struct Container {
            value: u32
        }
        
        fn make_container() -> Container %%%MARIGOLD_FUNCTION_START%%%
            Container { value: 0 }
        %%%MARIGOLD_FUNCTION_END%%%
        
        fn get_value(container: Container) -> u32 %%%MARIGOLD_FUNCTION_START%%%
            container.value
        %%%MARIGOLD_FUNCTION_END%%%
        
        range(1, 3).return
    ).await.collect::<Vec<i32>>().await;
    println!("Two functions result: {:?}", result2);
}
# Revision history for hierarchical-env

## 0.2.0.2 -- 2021-05-13
* Add `Eq` `Ord` `Show` instances for `Extends`

## 0.2.0.1 -- 2021-05-08
* Relax library dependency (`method`)

## 0.2.0.0 -- 2021-04-28
* Change how to specify the super environment
  * Before: Specify by adding type instance for `Super env`
    
    ```haskell
    data Env = Env !BaseEnv !Int
    deriveEnv ''Env
    type instance Super Env = BaseEnv
    ```
  * After: Wrapping the super environment field with `Extends`

    ```haskell
    data Env = Env !(Extends BaseEnv) !Int
    deriveEnv ''Env
    ```
* Fix the type error bug occurs when hiding dependencies.


## 0.1.0.0 -- 2021-04-22

* First version. Released on an unsuspecting world.

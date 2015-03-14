The new version should allows to define which type of injection each field require.

---

```
type
  TMyService = class
	public
	  [InjectByAlias('service_alias_name')]
	  property OtherService1: IOtherService1;
	  [InjectByType(IOtherService2Descendant)]	  
	  property OtherService2: IOtherService2;
	  //No attribute means "inject this prop as usual"
	  property OtherService3: IOtherService3;
	  [DoNotInject]
	  property OtherService4: IOtherService4;	  
  end;
```
# Timer Monitor

The Timer monitors the local system time and will trigger a `tick` event once 
per minute.

## Events:

The event structure produced by this Monitor is an Erlang tuple:

* `{tick, Minutes, Hours, Day, Month, DayOfWeek}` - Where all values are
    integers. The Day of the Week is zero indexed (i.e. 0 is Sunday).

## Example Usage:

Via the wiring configuration, you can simply do the following:

```erlang
{monitor, timer, libemp_timer_monitor}.
```


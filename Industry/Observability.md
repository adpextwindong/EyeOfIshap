# Observability

## [Ep. #53, Seduced by Reality with Jean Yang of Akita Software](https://www.heavybit.com/library/podcasts/o11ycast/ep-53-seduced-by-reality-with-jean-yang-of-akita-software/)
```
To me, logs are like assembly. They're low level, you can do anything with them, but what can we extract from logs?

...
And so where can we raise the abstraction from logs metrics and traces?
```

## Nginx x-request-id demo

- Leverage Nginx x-request-id for request tracing throughout systems
- Middleware on application side on recieving requests from the Nginx layer, and any requests we make over the network.

- Take ideas from Stella Cotton's USENIX talk [LISA17 - Distributed Tracing: From Theory to Practice](https://www.youtube.com/watch?v=fwVFshJz1Gs) and [So, you want to trace your distributed system? Key design insights from years of practical experience](https://www.pdl.cmu.edu/ftp/SelfStar/CMU-PDL-14-102.pdf)
  - Logical clock increment over context switches.

### Modes to build out in the test bed

- Simple CRUD operations
- OOM
- Clock skew/clock drift
- Message dropping (No response comes back)
- Failure of async callbacks from other servers like JDE
- DB call fails/transaction failure
- Backtracking in user flow
- User side input errors


Should user actions be totally reconstructable from server + client side logs?

### Spare Ideas

- Assigning positive/negative blame when we interact with systems we can log into. See Phil Wadler's talk [Faith, Evolution, and Programming Languages](https://www.youtube.com/watch?v=8frGknO8rIg)

- Completeness of middleware logging, all subrequests spawned (and db calls) should be logged.

- Log standardization

- Logs should have a Show/Read instance
  - Tool, time, person who did it, role/roles, user flow tag

#### Considerations

- User Impersonation for administration/debugging

#### Abstractions

User flows

Ideally we could unify against a user id and actions. A user flow could have a tag that bundles multiple fresh requests across what they're doing.

### Questions

Is a x-request-root-id header a sound idea.

Given a brand new "root" request, any subrequests throughout the system will inherit the root's x-request-id as x-request-root-id.

Is this header possible to maintain and preserve?

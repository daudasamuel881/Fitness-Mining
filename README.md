# Fitness Mining Smart Contract

A Stacks blockchain smart contract that rewards users with tokens for verified physical activities.

## Overview

The Fitness Mining contract allows users to:
- Submit their physical activities
- Earn tokens when activities are verified by authorized verifiers
- Track their fitness achievements on-chain

## Contract Features

- Submit activity records with type and duration
- Authorized verifiers can validate activities
- Automatic token minting upon verification
- Configurable reward amounts
- Activity claim prevention system

## Usage

### For Users
1. Submit activity:
```clarity
(contract-call? .fitness-mining submit-activity "running" u45 u1234567890)
```

### For Verifiers
1. Verify activity:
```clarity
(contract-call? .fitness-mining verify-activity 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM u1234567890)
```

### For Contract Owner
1. Add verifier:
```clarity
(contract-call? .fitness-mining add-verifier 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM)
```

2. Set tokens per activity:
```clarity
(contract-call? .fitness-mining set-tokens-per-activity u100)
```

## Requirements
- Clarinet
- Stacks blockchain wallet
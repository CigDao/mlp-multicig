import Principal "mo:base/Principal";
import Utils "../helpers/Utils";
import Nat64 "mo:base/Nat64";
import Constants "../Constants";

module {

    public type TxReceipt = {
        #Ok: Nat;
        #Err: {
            #InsufficientAllowance;
            #InsufficientBalance;
            #ErrorOperationStyle;
            #Unauthorized;
            #LedgerTrap;
            #ErrorTo;
            #Other: Text;
            #BlockUsed;
            #ActiveProposal;
            #AmountTooSmall;
        };
    };

    public func allowance(owner:Principal, spender:Principal, canisterId:Text): async Nat {
        let canister = actor(canisterId) : actor { 
            allowance : shared query (Principal, Principal) -> async Nat;
        };
        await canister.allowance(owner, spender);
    };

    public func transfer(to:Principal, amount:Nat, canisterId:Text): async TxReceipt {
        let canister = actor(canisterId) : actor { 
            transfer: (Principal, Nat)  -> async TxReceipt;
        };
        await canister.transfer(to, amount);
    };

    public func transferFrom(from:Principal, to:Principal, amount:Nat, canisterId:Text): async TxReceipt {
        let canister = actor(canisterId) : actor { 
            transferFrom : shared (Principal, Principal, Nat) -> async TxReceipt;
        };
        await canister.transferFrom(from, to, amount);
    };

    public func totalSupply(canisterId:Text): async Nat {
        let canister = actor(canisterId) : actor { 
            totalSupply : shared query () -> async Nat;
        };
        await canister.totalSupply();
    };
}
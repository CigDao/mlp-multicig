import Int64 "mo:base/Int64";
import Nat64 "mo:base/Nat64";
import Nat "mo:base/Nat";
import Float "mo:base/Float";
import Array "mo:base/Array";
import HashMap "mo:base/HashMap";
import Iter "mo:base/Iter";
import Text "mo:base/Text";
import Char "mo:base/Char";
import Option "mo:base/Option";
import Prim "mo:prim";
import Int "mo:base/Int";
import Int32 "mo:base/Int32";
import Nat32 "mo:base/Nat32";
import JSON "JSON";
import Request "../multisig/models/Request";
import Blob "mo:base/Blob";
import TrieMap "mo:base/TrieMap";

module {

    private type JSON = JSON.JSON;
    private type Request = Request.Request;
    private type RequestDraft = Request.RequestDraft;
    private type Transfer = Request.Transfer;
    private type Member = Request.Member;
    private type Threshold = Request.Threshold;
    private type TransferDraft = Request.TransferDraft;
    private type MemberDraft = Request.MemberDraft;
    private type ThresholdDraft = Request.ThresholdDraft;

    public func natToFloat(value:Nat): Float {
        return Float.fromInt(value)
    };

    public func floatToNat(value:Float): Nat {
        let int = Float.toInt(value);
        let text = Int.toText(int);
        return textToNat(text);
    };
    
    public func includesText(string: Text, term: Text): Bool {
        let stringArray = Iter.toArray<Char>(toLowerCase(string).chars());
        let termArray = Iter.toArray<Char>(toLowerCase(term).chars());

        var i = 0;
        var j = 0;

        while (i < stringArray.size() and j < termArray.size()) {
            if (stringArray[i] == termArray[j]) {
                i += 1;
                j += 1;
                if (j == termArray.size()) { return true; }
            } else {
                i += 1;
                j := 0;
            }
        };
        false
    };

    public func toLowerCase(value: Text) : Text {
        let chars = Text.toIter(value);
        var lower = "";
        for (c: Char in chars) {
        lower := Text.concat(lower, Char.toText(Prim.charToLower(c)));
        };
        return lower;
    };
    
    public func nat32ToInt(value: Nat32): Int {
        let int32 = Int32.fromNat32(value);
        Int32.toInt(int32);
    };


    public func textToNat32( txt : Text) : Nat32 {
        assert(txt.size() > 0);
        let chars = txt.chars();

        var num : Nat32 = 0;
        for (v in chars){
            let charToNum = Char.toNat32(v)-48;
            assert(charToNum >= 0 and charToNum <= 9);
            num := num * 10 +  charToNum;          
        };

        num;
    };

    public func textToNat( txt : Text) : Nat {
        assert(txt.size() > 0);
        let chars = txt.chars();

        var num : Nat = 0;
        for (v in chars){
            let charToNum = Char.toNat32(v)-48;
            assert(charToNum >= 0 and charToNum <= 9);
            num := num * 10 +  Nat32.toNat(charToNum);          
        };

        num;
    };

    public func requestToJson(request: Request): JSON {
        switch(request){
            case(#transfer(value)){
                _transferToJson(value);
            };
            case(#addMember(value)){
                _memberToJson(value)
            };
            case(#removeMember(value)){
                _memberToJson(value)
            };
            case(#threshold(value)){
                _thresholdToJson(value);
            };
        };
    };

    public func requestDraftToJson(request: RequestDraft): JSON {
        switch(request){
            case(#transfer(value)){
                _transferDraftToJson(value);
            };
            case(#addMember(value)){
                _memberDraftToJson(value)
            };
            case(#removeMember(value)){
                _memberDraftToJson(value)
            };
            case(#threshold(value)){
                _thresholdDraftToJson(value);
            };
        };
    };

    private func _approvalToJson(value: TrieMap.TrieMap<Text, Nat>): [JSON] {
        var approvals:[JSON] = [];
        for((member, power) in value.entries()){
            let map : HashMap.HashMap<Text, JSON> = HashMap.HashMap<Text, JSON>(
                0,
                Text.equal,
                Text.hash,
            );
            map.put("member", #String(member));
            map.put("power", #Number(power));

            let json = #Object(map);
            approvals := Array.append(approvals,[json])
        };

        approvals;
        
    };

    private func _transferToJson(value: Transfer): JSON {
        var approvals:[JSON] = _approvalToJson(value.approvals);
        let map : HashMap.HashMap<Text, JSON> = HashMap.HashMap<Text, JSON>(
            0,
            Text.equal,
            Text.hash,
        );

        let executedAt = value.executedAt;
        switch(executedAt){
            case(?executedAt){
                map.put("executedAt", #Number(executedAt));
            };
            case(null) {

            };
        };

        map.put("id", #Number(Nat32.toNat(value.id)));
        map.put("amount", #Number(value.amount));
        map.put("recipient", #String(value.recipient));
        map.put("approvals", #Array(approvals));
        map.put("executed", #Boolean(value.executed));
        map.put("createdAt", #Number(value.createdAt));
        map.put("description", #String(value.description));

        #Object(map);
    };

    private func _transferDraftToJson(value: TransferDraft): JSON {
        let map : HashMap.HashMap<Text, JSON> = HashMap.HashMap<Text, JSON>(
            0,
            Text.equal,
            Text.hash,
        );
        map.put("amount", #Number(value.amount));
        map.put("recipient", #String(value.recipient));
        map.put("description", #String(value.description));

        #Object(map);
    };

    private func _memberToJson(value: Member): JSON {
        var approvals:[JSON] = _approvalToJson(value.approvals);
        let map : HashMap.HashMap<Text, JSON> = HashMap.HashMap<Text, JSON>(
            0,
            Text.equal,
            Text.hash,
        );

        let executedAt = value.executedAt;
        switch(executedAt){
            case(?executedAt){
                map.put("executedAt", #Number(executedAt));
            };
            case(null) {

            };
        };

        map.put("id", #Number(Nat32.toNat(value.id)));
        map.put("principal", #String(value.principal));
        map.put("power", #Number(value.power));
        map.put("description", #String(value.description));
        map.put("approvals", #Array(approvals));
        map.put("executed", #Boolean(value.executed));
        map.put("createdAt", #Number(value.createdAt));

        #Object(map);
    };

    private func _memberDraftToJson(value: MemberDraft): JSON {
        let map : HashMap.HashMap<Text, JSON> = HashMap.HashMap<Text, JSON>(
            0,
            Text.equal,
            Text.hash,
        );

        map.put("principal", #String(value.principal));
        map.put("power", #Number(value.power));
        map.put("description", #String(value.description));

        #Object(map);
    };

    private func _thresholdToJson(value: Threshold): JSON {
        var approvals:[JSON] = _approvalToJson(value.approvals);
        let map : HashMap.HashMap<Text, JSON> = HashMap.HashMap<Text, JSON>(
            0,
            Text.equal,
            Text.hash,
        );

        let executedAt = value.executedAt;
        switch(executedAt){
            case(?executedAt){
                map.put("executedAt", #Number(executedAt));
            };
            case(null) {

            };
        };

        map.put("id", #Number(Nat32.toNat(value.id)));
        map.put("power", #Number(value.power));
        map.put("description", #String(value.description));
        map.put("approvals", #Array(approvals));
        map.put("executed", #Boolean(value.executed));
        map.put("createdAt", #Number(value.createdAt));

        #Object(map);
    };

    private func _thresholdDraftToJson(value: ThresholdDraft): JSON {
        let map : HashMap.HashMap<Text, JSON> = HashMap.HashMap<Text, JSON>(
            0,
            Text.equal,
            Text.hash,
        );
        map.put("power", #Number(value.power));
        map.put("description", #String(value.description));
        #Object(map);
    };
}
package org.siemac.metamac.statistical.operations.web.server.stream;

import org.apache.avro.specific.SpecificRecordBase;

public class AvroMessage<K, V extends SpecificRecordBase> extends MessageBase<K, V> {
    public AvroMessage(K key, V messageContent) {
        super(key, messageContent);
    }
}

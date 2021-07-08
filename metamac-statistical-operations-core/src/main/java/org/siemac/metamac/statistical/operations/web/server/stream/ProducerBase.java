package org.siemac.metamac.statistical.operations.web.server.stream;

import org.siemac.metamac.core.common.exception.MetamacException;

public interface ProducerBase<K, V> {
    void sendMessage(MessageBase<K, V> m, String topic) throws MetamacException;
    void close();
}

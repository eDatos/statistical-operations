package org.siemac.metamac.statistical.operations.web.shared;

import java.io.Serializable;
import java.util.List;

import org.siemac.metamac.core.common.dto.ExternalItemDto;

public class ExternalItemsResult implements Serializable {

    private static final long     serialVersionUID = 1L;

    private List<ExternalItemDto> externalItemDtos;
    private int                   firstResult;
    private int                   totalResults;

    public ExternalItemsResult() {
    }

    public ExternalItemsResult(List<ExternalItemDto> externalItemDtos, int firstResult, int totalResults) {
        this();
        this.externalItemDtos = externalItemDtos;
        this.firstResult = firstResult;
        this.totalResults = totalResults;
    }

    public List<ExternalItemDto> getExternalItemDtos() {
        return externalItemDtos;
    }

    public int getFirstResult() {
        return firstResult;
    }

    public int getTotalResults() {
        return totalResults;
    }

    public void setExternalItemDtos(List<ExternalItemDto> externalItemDtos) {
        this.externalItemDtos = externalItemDtos;
    }

    public void setFirstResult(int firstResult) {
        this.firstResult = firstResult;
    }

    public void setTotalResults(int totalResults) {
        this.totalResults = totalResults;
    }
}

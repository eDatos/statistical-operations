package org.siemac.metamac.statistical.operations.web.server.utils;

import java.util.ArrayList;
import java.util.List;

import org.siemac.metamac.core.common.dto.ExternalItemDto;
import org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum;
import org.siemac.metamac.rest.common.v1_0.domain.ListBase;
import org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.Categories;
import org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.CategorySchemes;
import org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.ResourceInternal;
import org.siemac.metamac.statistical.operations.web.shared.ExternalItemsResult;
import org.siemac.metamac.web.common.server.utils.DtoUtils;

public class ExternalItemUtils extends org.siemac.metamac.web.common.client.utils.ExternalItemUtils {

    //
    // CATEGORIES
    //

    // Category Schemes

    public static ExternalItemsResult getCategorySchemesAsExternalItemsResult(CategorySchemes categorySchemes) {
        ExternalItemsResult result = getListBaseAsExternalItemsResult(categorySchemes);
        result.setExternalItemDtos(getCategorySchemesAsExternalItemDtos(categorySchemes.getCategorySchemes()));
        return result;
    }

    public static ExternalItemDto getCategorySchemeAsExternalItemDto(ResourceInternal resourceInternal) {
        ExternalItemDto externalItemDto = getResourceInternalAsExternalItemDto(resourceInternal);
        externalItemDto.setType(TypeExternalArtefactsEnum.CATEGORY_SCHEME);
        return externalItemDto;
    }

    public static List<ExternalItemDto> getCategorySchemesAsExternalItemDtos(List<ResourceInternal> resources) {
        List<ExternalItemDto> externalItemDtos = new ArrayList<ExternalItemDto>(resources.size());
        for (ResourceInternal resource : resources) {
            externalItemDtos.add(getCategorySchemeAsExternalItemDto(resource));
        }
        return externalItemDtos;
    }

    // Categories

    public static ExternalItemsResult getCategoriesAsExternalItemsResult(Categories categories) {
        ExternalItemsResult result = getListBaseAsExternalItemsResult(categories);
        result.setExternalItemDtos(getCategoriesAsExternalItemDtos(categories.getCategories()));
        return result;
    }

    public static ExternalItemDto getCategoryAsExternalItemDto(ResourceInternal resourceInternal) {
        ExternalItemDto externalItemDto = getResourceInternalAsExternalItemDto(resourceInternal);
        externalItemDto.setType(TypeExternalArtefactsEnum.CATEGORY);
        return externalItemDto;
    }

    public static List<ExternalItemDto> getCategoriesAsExternalItemDtos(List<ResourceInternal> resources) {
        List<ExternalItemDto> externalItemDtos = new ArrayList<ExternalItemDto>(resources.size());
        for (ResourceInternal resource : resources) {
            externalItemDtos.add(getCategoryAsExternalItemDto(resource));
        }
        return externalItemDtos;
    }

    //
    // COMMON METHODS
    //

    private static ExternalItemsResult getListBaseAsExternalItemsResult(ListBase listBase) {
        ExternalItemsResult result = new ExternalItemsResult();
        if (listBase != null) {
            result.setFirstResult(listBase.getOffset() != null ? listBase.getOffset().intValue() : 0);
            result.setTotalResults(listBase.getOffset() != null ? listBase.getTotal().intValue() : 0);
        }
        return result;
    }

    private static ExternalItemDto getResourceInternalAsExternalItemDto(ResourceInternal resourceInternal) {
        ExternalItemDto externalItemDto = new ExternalItemDto();
        externalItemDto.setCode(resourceInternal.getId());
        externalItemDto.setUri(resourceInternal.getSelfLink().getHref());
        externalItemDto.setUrn(resourceInternal.getUrn());
        externalItemDto.setTitle(DtoUtils.getInternationalStringDtoFromInternationalString(resourceInternal.getTitle()));
        externalItemDto.setManagementAppUrl(resourceInternal.getManagementAppLink());
        return externalItemDto;
    }
}
